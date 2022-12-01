extern crate core;

use std::fs;

enum Part {
    One,
    Two,
}

impl TryInto<Part> for usize {
    type Error = ();

    fn try_into(self) -> Result<Part, Self::Error> {
        match self {
            1 => Ok(Part::One),
            2 => Ok(Part::Two),
            _ => Err(()),
        }
    }
}
/// solve problem for day 1
fn day1(input: &str, part: Part) -> usize {
    /// Takes a list of inventories, separated by blank lies, of calories, separated by newlines, as input.
    /// Returns the sum of the one containing the most calories.
    fn part1(input: &str) -> usize {
        input
            .split("\n\n")
            .map(|inventory_as_string| {
                inventory_as_string
                    .split('\n')
                    .filter_map(|calories| calories.parse::<usize>().ok())
                    .sum::<usize>()
            })
            .max()
            .unwrap()
    }

    /// Takes a list of inventories, separated by blank lies, of calories, separated by newlines, as input.
    /// Returns the top 3 largest inventory sums.
    fn part2(input: &str) -> usize {
        let mut max = [0, 0, 0];
        for calories in input.split("\n\n").map(|inventory_as_string| {
            inventory_as_string
                .split('\n')
                .filter_map(|calories| calories.parse::<usize>().ok())
                .sum::<usize>()
        }) {
            if calories > max[0] {
                max[0] = calories;
                max.sort();
            }
        }
        max.iter().sum()
    }

    match part {
        Part::One => part1(input),
        Part::Two => part2(input),
    }
}

/// passes problem input to solver for the given day
fn main() {
    let contents = fs::read_to_string("./input")
        .expect("where's the input file? didn't find it at './input'.");
    let solution = day1(&contents, Part::Two);
    println!("Solution: {}", solution)
}
