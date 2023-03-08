use std::iter::Map;
use std::str::Split;

use crate::{Part, Solution};

/// Takes a list of inventories, separated by blank lines, of calories, separated by newlines, as input.
/// Returns an iterator over the sum of each inventory.
/// (common sub-problem for both parts of day 1)
fn parse_inventory_totals(input: &str) -> Map<Split<&str>, fn(&str) -> usize> {
    input.split("\n\n").map(|inventory_as_string| {
        inventory_as_string
            .split('\n')
            // parse is sometimes being fed something empty here, I don't know why
            // but just ignoring it gives the correct result anyways
            // so it's probably the nested splitting that is leaving behind an empty string or something
            .flat_map(|calories| calories.parse::<usize>())
            .sum()
    })
}

/// solve problem for day 1
pub fn day1(input: &str, part: Part) -> Solution {
    /// Takes a list of inventories, separated by blank lines, of calories, separated by newlines, as input.
    /// Returns the sum of the one containing the most calories.
    fn part1(input: &str) -> usize {
        parse_inventory_totals(input)
            .max()
            .expect("couldn't calculate max!")
    }

    /// Takes a list of inventories, separated by blank lines, of calories, separated by newlines, as input.
    /// Returns the sum of the top 3 largest inventory sums.
    fn part2(input: &str) -> usize {
        let mut max = [0, 0, 0];
        for calories in parse_inventory_totals(input) {
            // sorting guarantees that if calories is bigger than max[0] then it is among the top 3.
            // additionally, sorting guarantees that max[0] is the smallest and thus should always be dropped when a new max is found.
            if calories > max[0] {
                max[0] = calories;
                max.sort();
            }
        }
        max.iter().sum()
    }

    match part {
        Part::One => Solution::USize(part1(input)),
        Part::Two => Solution::USize(part2(input)),
    }
}
