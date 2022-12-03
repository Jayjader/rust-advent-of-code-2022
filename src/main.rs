extern crate core;

use std::collections::{HashMap, HashSet};
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
                // parse is sometimes being fed something empty here, I don't know why
                // but just ignoring it gives the correct result anyways
                // so it's probably the nested splitting that is leaving behind an empty string or something
                .flat_map(|calories| calories.parse::<usize>())
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
        Part::One => part1(input),
        Part::Two => part2(input),
    }
}

/// solve problem for day 2
fn day2(input: &str, part: Part) -> u32 {
    enum Shape {
        Rock,
        Paper,
        Scissors,
    }
    enum Outcome {
        Lose,
        Draw,
        Win,
    }

    impl TryInto<Shape> for char {
        type Error = char;

        fn try_into(self) -> Result<Shape, Self::Error> {
            match self {
                'A' => Ok(Shape::Rock),
                'B' => Ok(Shape::Paper),
                'C' => Ok(Shape::Scissors),
                'X' => Ok(Shape::Rock),
                'Y' => Ok(Shape::Paper),
                'Z' => Ok(Shape::Scissors),
                c => Err(c),
            }
        }
    }
    impl TryInto<Outcome> for char {
        type Error = char;

        fn try_into(self) -> Result<Outcome, Self::Error> {
            match self {
                'X' => Ok(Outcome::Lose),
                'Y' => Ok(Outcome::Draw),
                'Z' => Ok(Outcome::Win),
                c => Err(c),
            }
        }
    }
    trait Scored {
        fn score(thing: &Self) -> u32;
    }

    impl Scored for Shape {
        fn score(s: &Shape) -> u32 {
            match s {
                Shape::Rock => 1,
                Shape::Paper => 2,
                Shape::Scissors => 3,
            }
        }
    }
    impl Scored for Outcome {
        fn score(o: &Outcome) -> u32 {
            match o {
                Outcome::Lose => 0,
                Outcome::Draw => 3,
                Outcome::Win => 6,
            }
        }
    }
    fn part1(input: &str) -> u32 {
        input.lines().fold(0, |accu, line| {
            let mut chars = line.chars();
            let opponent_move: Shape = chars
                .next()
                .expect("line missing first character")
                .try_into()
                .expect("character is not shape");
            chars.next().expect("line missing second character");
            let my_move: Shape = chars
                .next()
                .expect("line missing third character")
                .try_into()
                .expect("character is not outcome");
            let outcome = match (&my_move, &opponent_move) {
                (Shape::Rock, Shape::Rock) => Outcome::Draw,
                (Shape::Paper, Shape::Paper) => Outcome::Draw,
                (Shape::Scissors, Shape::Scissors) => Outcome::Draw,
                (Shape::Rock, Shape::Paper) => Outcome::Lose,
                (Shape::Paper, Shape::Rock) => Outcome::Win,
                (Shape::Scissors, Shape::Paper) => Outcome::Win,
                (Shape::Paper, Shape::Scissors) => Outcome::Lose,
                (Shape::Rock, Shape::Scissors) => Outcome::Win,
                (Shape::Scissors, Shape::Rock) => Outcome::Lose,
            };
            accu + Scored::score(&outcome) + Scored::score(&my_move)
        })
    }
    fn part2(input: &str) -> u32 {
        input.lines().fold(0, |accu, line| {
            let mut chars = line.chars();
            let opponent_move: Shape = chars
                .next()
                .expect("line missing first character")
                .try_into()
                .expect("character is not shape");
            chars.next().expect("line missing second character");
            let desired_outcome: Outcome = chars
                .next()
                .expect("line missing third character")
                .try_into()
                .expect("character is not outcome");
            let my_move = match (&opponent_move, &desired_outcome) {
                (Shape::Rock, Outcome::Lose) => Shape::Scissors,
                (Shape::Rock, Outcome::Draw) => Shape::Rock,
                (Shape::Rock, Outcome::Win) => Shape::Paper,
                (Shape::Paper, Outcome::Lose) => Shape::Rock,
                (Shape::Paper, Outcome::Draw) => Shape::Paper,
                (Shape::Paper, Outcome::Win) => Shape::Scissors,
                (Shape::Scissors, Outcome::Lose) => Shape::Paper,
                (Shape::Scissors, Outcome::Draw) => Shape::Scissors,
                (Shape::Scissors, Outcome::Win) => Shape::Rock,
            };
            accu + Scored::score(&desired_outcome) + Scored::score(&my_move)
        })
    }
    match part {
        Part::One => part1(input),
        Part::Two => part2(input),
    }
}

/// solve problem for day 3
fn day3(input: &str, part: Part) -> usize {
    fn part1(input: &str) -> usize {
        input
            .lines()
            .map(|rucksack| {
                let (compartment_1, compartment_2) = rucksack.split_at(rucksack.len() / 2);
                let (compartment_1, compartment_2) = (
                    compartment_1.chars().collect::<HashSet<_>>(),
                    compartment_2.chars().collect::<HashSet<_>>(),
                );
                let shared: Vec<&char> = compartment_1.intersection(&compartment_2).collect();
                assert_eq!(shared.len(), 1);
                let shared_char = shared[0];

                (*shared_char as u32 as usize)
                    - (if shared_char.is_uppercase() {
                        65 - 27 // 65 is 'A'
                    } else {
                        97 - 1 // 97 is 'a'
                    })
            })
            .sum()
    }
    match part {
        Part::One => part1(input),
        Part::Two => 0,
    }
}

/// passes problem input to solver for the given day
fn main() -> Result<(), Box<dyn Error>> {
    let contents =
        fs::read_to_string("./input").expect("where's the input file? didn't find it at './input'");
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        let part1 = day3(&contents, Part::One);
        let part2 = day3(&contents, Part::Two);
        Ok(println!("Solutions:\nPart 1: {}, Part 2: {}", part1, part2))
    } else {
        let part = args[1].parse::<usize>()?.try_into()?;
        let solution = day3(&contents, part);
        Ok(println!("Solution: {}", solution))
    }
}
