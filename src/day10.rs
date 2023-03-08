use std::str::FromStr;

use crate::{Part, Solution};

#[derive(Debug)]
enum Instruction {
    Noop,
    AddX(i32),
}
impl FromStr for Instruction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.chars().take(4).collect::<String>().as_str() {
            "noop" => Ok(Instruction::Noop),
            "addx" => Ok(Instruction::AddX(s.split_at(5).1.parse::<i32>().unwrap())),
            _ => Err(()),
        }
    }
}

/// solves the problem for day 10
pub fn day10(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> isize {
        let (_, _, reg_per_cycle) = input
            .lines()
            .flat_map(|line| line.parse::<Instruction>())
            .fold(
                (0, 1, Vec::<i32>::with_capacity(240)),
                |(cycle, register, mut output), instruction| match instruction {
                    Instruction::Noop => {
                        output.push(register);
                        (cycle + 1, register, output)
                    }
                    Instruction::AddX(x) => {
                        output.push(register);
                        output.push(register);
                        (cycle + 2, register + x, output)
                    }
                },
            );
        (20 * reg_per_cycle[20 - 1]
            + 60 * reg_per_cycle[60 - 1]
            + 100 * reg_per_cycle[100 - 1]
            + 140 * reg_per_cycle[140 - 1]
            + 180 * reg_per_cycle[180 - 1]
            + 220 * reg_per_cycle[220 - 1]) as isize
    }
    fn part2(input: &str) -> String {
        fn screen_coords_during_cycle(cycle: usize) -> (usize, usize) {
            (cycle % 40, cycle / 40)
        }
        let mut screen = vec![vec!["."; 40]; 6];
        let (_, _, _) = input
            .lines()
            .flat_map(|line| line.parse::<Instruction>())
            .fold(
                (0isize, 1isize, Vec::<isize>::with_capacity(240)),
                |(cycle, register, mut output), instruction| {
                    let (x, y) = screen_coords_during_cycle(cycle as usize);
                    if [register - 1, register, register + 1].contains(&(x as isize)) {
                        screen[y][x] = "#"
                    }
                    match instruction {
                        Instruction::Noop => {
                            output.push(register);
                            (cycle + 1, register, output)
                        }
                        Instruction::AddX(x_arg) => {
                            output.push(register);
                            output.push(register);
                            let (x, y) = screen_coords_during_cycle((cycle + 1) as usize);
                            if [register - 1, register, register + 1].contains(&(x as isize)) {
                                screen[y][x] = "#"
                            }
                            (cycle + 2, register + x_arg as isize, output)
                        }
                    }
                },
            );
        screen
            .iter()
            .map(|line| line.join(""))
            .collect::<Vec<_>>()
            .join("\n")
    }

    match part {
        Part::One => Solution::INumber(part1(input)),
        Part::Two => Solution::String(part2(input)),
    }
}
