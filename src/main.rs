extern crate core;

use crate::Outcome::{Draw, Lose, Win};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Map;
use std::str::Split;
use std::{env, fs};
use Shape::{Paper, Rock, Scissors};

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
            'A' => Some(Rock),
            'B' => Some(Paper),
            'C' => Some(Scissors),
            'X' => Some(Rock),
            'Y' => Some(Paper),
            'Z' => Some(Scissors),
            _ => None,
        })
        .ok_or(())
    }
}
impl TryInto<Outcome> for char {
    type Error = ();

    fn try_into(self) -> Result<Outcome, Self::Error> {
        (match self {
            'X' => Some(Lose),
            'Y' => Some(Draw),
            'Z' => Some(Win),
            _ => None,
        })
        .ok_or(())
    }
}
fn outcome(me: &Shape, opponent: &Shape) -> Outcome {
    match (me, opponent) {
        (&Rock, &Rock) => Draw,
        (&Paper, &Paper) => Draw,
        (&Scissors, &Scissors) => Draw,
        (&Rock, &Paper) => Lose,
        (&Paper, &Rock) => Win,
        (&Scissors, &Paper) => Win,
        (&Paper, &Scissors) => Lose,
        (&Rock, &Scissors) => Win,
        (&Scissors, &Rock) => Lose,
    }
}
fn move_needed(opponent_move: &Shape, desired_outcome: &Outcome) -> Shape {
    match (opponent_move, desired_outcome) {
        (&Rock, &Lose) => Scissors,
        (&Rock, &Draw) => Rock,
        (&Rock, &Win) => Paper,
        (&Paper, &Lose) => Rock,
        (&Paper, &Draw) => Paper,
        (&Paper, &Win) => Scissors,
        (&Scissors, &Lose) => Paper,
        (&Scissors, &Draw) => Scissors,
        (&Scissors, &Win) => Rock,
    }
}
trait Scored {
    fn score(thing: &Self) -> u32;
}

impl Scored for Shape {
    fn score(s: &Shape) -> u32 {
        match s {
            Rock => 1,
            Paper => 2,
            Scissors => 3,
        }
    }
}
impl Scored for Outcome {
    fn score(o: &Outcome) -> u32 {
        match o {
            Lose => 0,
            Draw => 3,
            Win => 6,
        }
    }
}

/// solve problem for day 2
fn day2(input: &str, part: Part) -> u32 {
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
            let outcome = outcome(&my_move, &opponent_move);
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
            let my_move = move_needed(&opponent_move, &desired_outcome);
            let outcome = outcome(&my_move, &opponent_move);
            accu + Scored::score(&outcome) + Scored::score(&my_move)
        })
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
        let part1 = day2(&contents, Part::One);
        let part2 = day2(&contents, Part::Two);
        Ok(println!("Solutions:\nPart 1: {}, Part 2: {}", part1, part2))
    } else {
        let part = args[1].parse::<usize>()?.try_into()?;
        let solution = day2(&contents, part);
        Ok(println!("Solution: {}", solution))
    }
}
