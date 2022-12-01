extern crate core;

use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Map;
use std::str::Split;
use std::{env, fs};

enum Part {
    One,
    Two,
}

#[derive(Debug, Clone)]
struct PartParseError(usize);

impl TryInto<Part> for usize {
    type Error = PartParseError;

    fn try_into(self) -> Result<Part, Self::Error> {
        match self {
            1 => Ok(Part::One),
            2 => Ok(Part::Two),
            _ => Err(PartParseError(self)),
        }
    }
}

impl Display for PartParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "couldn't parse problem part '{}'", self.0)
    }
}

impl Error for PartParseError {}

/// solve problem for day 1
fn day1(input: &str, part: Part) -> usize {
    /// Takes a list of inventories, separated by blank lines, of calories, separated by newlines, as input.
    /// Returns an iterator over the sum of each inventory.
    /// (common sub-problem for both parts of day 1)
    fn parse_inventory_totals(input: &str) -> Map<Split<&str>, fn(&str) -> usize> {
        input.split("\n\n").map(|inventory_as_string| {
            inventory_as_string
                .split('\n')
                .filter_map(|calories| calories.parse::<usize>().ok())
                .sum()
        })
    }

    /// Takes a list of inventories, separated by blank lines, of calories, separated by newlines, as input.
    /// Returns the sum of the one containing the most calories.
    fn part1(input: &str) -> usize {
        parse_inventory_totals(input)
            .max()
            .expect("couldn't calculate max!")
    }

    /// Takes a list of inventories, separated by blank lines, of calories, separated by newlines, as input.
    /// Returns the top 3 largest inventory sums.
    fn part2(input: &str) -> usize {
        let mut max = [0, 0, 0];
        for calories in parse_inventory_totals(input) {
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
fn main() -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string("./input")
        .expect("where's the input file? didn't find it at './input'.");
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        let part1 = day1(&contents, Part::One);
        let part2 = day1(&contents, Part::Two);
        println!("Solutions:\nPart 1: {}, Part 2: {}", part1, part2);
        Ok(())
    } else {
        let part = args[1].parse::<usize>()?.try_into()?;
        let solution = day1(&contents, part);
        Ok(println!("Solution: {}", solution))
    }
}
