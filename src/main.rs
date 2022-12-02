extern crate core;

use crate::Outcome::{Draw, Lose, Win};
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

#[derive(Debug)]
enum Shape {
    Rock,
    Paper,
    Scissors,
}
#[derive(Debug)]
enum Outcome {
    Lose,
    Draw,
    Win,
}

impl TryInto<Shape> for char {
    type Error = ();

    fn try_into(self) -> Result<Shape, Self::Error> {
        (match self {
            'A' => Some(Shape::Rock),
            'B' => Some(Shape::Paper),
            'C' => Some(Shape::Scissors),
            'X' => Some(Shape::Rock),
            'Y' => Some(Shape::Paper),
            'Z' => Some(Shape::Scissors),
            _ => None,
        })
        .ok_or(())
    }
}
fn outcome(me: &Shape, opponent: &Shape) -> Outcome {
    match (me, opponent) {
        (&Shape::Rock, &Shape::Rock) => Draw,
        (&Shape::Paper, &Shape::Paper) => Draw,
        (&Shape::Scissors, &Shape::Scissors) => Draw,
        (&Shape::Rock, &Shape::Paper) => Lose,
        (&Shape::Paper, &Shape::Rock) => Win,
        (&Shape::Scissors, &Shape::Paper) => Win,
        (&Shape::Paper, &Shape::Scissors) => Lose,
        (&Shape::Rock, &Shape::Scissors) => Win,
        (&Shape::Scissors, &Shape::Rock) => Lose,
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

/// solve problem for day 2
fn day2(input: &str, part: Part) -> u32 {
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
        let outcome = outcome(&my_move, &opponent_move);
        println!(
            "{:?} ({}) + {:?} = {:?} ({})",
            my_move,
            Scored::score(&my_move),
            opponent_move,
            outcome,
            Scored::score(&outcome)
        );
        accu + Scored::score(&outcome) + Scored::score(&my_move)
    })
}

/// passes problem input to solver for the given day
fn main() -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string("./input.real")
        .expect("where's the input file? didn't find it at './input'.");
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        let part1 = day2(&contents, Part::One);
        let part2 = day2(&contents, Part::Two);
        Ok(println!("Solutions:\nPart 1: {}, Part 2: {}", part1, part2))
    } else {
        let part = args[1].parse::<usize>()?.try_into()?;
        let solution = day2(&contents, part);
        Ok(println!("Solution: {}", solution))
    }
}
