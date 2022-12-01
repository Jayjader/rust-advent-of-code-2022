extern crate core;

use std::fs;
/// solve problem for day 1
fn day1(input: &str) -> usize {
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

    part1(input)
}

/// passes problem input to solver for the given day
fn main() {
    let contents = fs::read_to_string("./input")
        .expect("where's the input file? didn't find it at './input'.");
    let solution = day1(&contents);
    println!("Solution: {}", solution)
}
