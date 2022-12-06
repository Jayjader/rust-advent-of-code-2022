extern crate core;

use std::collections::{HashSet, VecDeque};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Map;
use std::str::Split;
use std::{env, fs};

use regex::Regex;

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

#[derive(Debug)]
enum Solution {
    Number(usize),
    String(String),
}
impl Display for Solution {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Solution::Number(n) => {
                write!(f, "{}", n)
            }
            Solution::String(s) => {
                write!(f, "{}", s)
            }
        }
    }
}

/// solve problem for day 1
fn day1(input: &str, part: Part) -> Solution {
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
        Part::One => Solution::Number(part1(input)),
        Part::Two => Solution::Number(part2(input)),
    }
}

/// solve problem for day 2
fn day2(input: &str, part: Part) -> Solution {
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
        fn score(thing: &Self) -> usize;
    }

    impl Scored for Shape {
        fn score(s: &Shape) -> usize {
            match s {
                Shape::Rock => 1,
                Shape::Paper => 2,
                Shape::Scissors => 3,
            }
        }
    }
    impl Scored for Outcome {
        fn score(o: &Outcome) -> usize {
            match o {
                Outcome::Lose => 0,
                Outcome::Draw => 3,
                Outcome::Win => 6,
            }
        }
    }
    fn part1(input: &str) -> usize {
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
    fn part2(input: &str) -> usize {
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
        Part::One => Solution::Number(part1(input)),
        Part::Two => Solution::Number(part2(input)),
    }
}

/// solve problem for day 3
fn day3(input: &str, part: Part) -> Solution {
    /// common sub-problem for both parts of day 3
    fn priority_for_char(shared_char: &char) -> usize {
        (*shared_char as u32 as usize)
            - (if shared_char.is_uppercase() {
                65 - 27 // 65 is 'A' which has priority 26
            } else {
                97 - 1 // 97 is 'a' which has priority 1
            })
    }
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
                // safety check that there really is 1 and only 1 shared char
                assert_eq!(shared.len(), 1);

                priority_for_char(shared[0])
            })
            .sum()
    }

    fn part2(input: &str) -> usize {
        // collect into intermediate Vec so that we can call .chunks_exact on it
        // in nightly we could use array_chunk or next_chunk to avoid this intermediate allocation
        let vec_lines: Vec<_> = input.lines().collect();

        vec_lines
            .chunks_exact(3)
            .map(|group_of_rucksacks| {
                group_of_rucksacks
                    .iter()
                    .map(|rucksack| rucksack.chars().collect::<HashSet<char>>())
            })
            .map(|hashed_sacks| {
                let mut common = hashed_sacks.clone().next().unwrap();
                for sack in hashed_sacks {
                    common = common.intersection(&sack).copied().collect();
                }
                // safety check that we found a single common element for each group of 3 rucksacks
                assert_eq!(common.len(), 1);
                common
            })
            .map(|c| priority_for_char(c.iter().collect::<Vec<_>>()[0]))
            .sum()
    }
    match part {
        Part::One => Solution::Number(part1(input)),
        Part::Two => Solution::Number(part2(input)),
    }
}

/// solves problem for day 4
fn day4(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> usize {
        input
            .lines()
            .flat_map(|line| line.split_once(','))
            .map(|(section1, section2)| {
                (
                    section1.split_once('-').unwrap(),
                    section2.split_once('-').unwrap(),
                )
            })
            .map(|((a0, a1), (b0, b1))| {
                (
                    (a0.parse::<usize>().unwrap(), a1.parse::<usize>().unwrap()),
                    (b0.parse::<usize>().unwrap(), b1.parse::<usize>().unwrap()),
                )
            })
            .map(|((start1, end1), (start2, end2))| {
                // bool condition to unsigned int: true is 1 and false is 0
                usize::from(start1 <= start2 && end1 >= end2 || start2 <= start1 && end2 >= end1)
            })
            .sum()
    }

    fn part2(input: &str) -> usize {
        input
            .lines()
            .flat_map(|line| line.split_once(','))
            .map(|(section1, section2)| {
                (
                    section1.split_once('-').unwrap(),
                    section2.split_once('-').unwrap(),
                )
            })
            .map(|((a0, a1), (b0, b1))| {
                (
                    (a0.parse::<usize>().unwrap(), a1.parse::<usize>().unwrap()),
                    (b0.parse::<usize>().unwrap(), b1.parse::<usize>().unwrap()),
                )
            })
            .map(|((start1, end1), (start2, end2))| {
                // bool condition to unsigned int: true is 1 and false is 0
                usize::from(
                    start1 <= start2 && end1 >= start2 || start2 <= start1 && end2 >= start1,
                )
            })
            .sum()
    }

    match part {
        Part::One => Solution::Number(part1(input)),
        Part::Two => Solution::Number(part2(input)),
    }
}

/// solves problem for day 5
fn day5(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> String {
        let (drawing, commands) = input.split_once("\n\n").unwrap();
        let newline_index = drawing.find('\n').unwrap();
        // a drawing with n stacks has 4n-1 characters per line:
        // 3 per stack (3n) + in between the stacks (n-1)
        let number_of_stacks = (newline_index + 1) / 4;
        let mut stacks = Vec::with_capacity(number_of_stacks);
        stacks.resize_with(number_of_stacks, VecDeque::<char>::new);
        // skip the last line as we can just infer the stack indices
        let end_of_stacks = drawing.rfind('\n').unwrap();
        for line in drawing[0..end_of_stacks].lines() {
            for (index, c) in line.chars().enumerate() {
                // [S] [D]     [W]     [W]     [H] [Q]
                //  |   |       |
                // 0123012301230123...
                //  ^   ^       ^
                if index % 4 == 1 && c.is_alphabetic() {
                    stacks[index / 4].push_back(c)
                }
            }
        }
        // parse command
        let re = Regex::new(r"^move (?P<count>\d+) from (?P<start>\d+) to (?P<end>\d+)$").unwrap();
        for line in commands.lines() {
            if let Some(caps) = re.captures(line) {
                let count = caps["count"].parse::<usize>().unwrap();
                let start = caps["start"].parse::<usize>().unwrap();
                let end = caps["end"].parse::<usize>().unwrap();
                for _ in 0..count {
                    let moved = stacks[start - 1].pop_front().unwrap();
                    stacks[end - 1].push_front(moved);
                }
            }
        }
        stacks.iter().map(|stack| stack[0]).collect::<String>()
    }
    fn part2(input: &str) -> String {
        let (drawing, commands) = input.split_once("\n\n").unwrap();
        let newline_index = drawing.find('\n').unwrap();
        // a drawing with n stacks has 4n-1 characters per line:
        // 3 per stack (3n) + in between the stacks (n-1)
        let number_of_stacks = (newline_index + 1) / 4;
        let mut stacks = Vec::with_capacity(number_of_stacks);
        stacks.resize_with(number_of_stacks, VecDeque::<char>::new);
        // skip the last line as we can just infer the stack indices
        let end_of_stacks = drawing.rfind('\n').unwrap();
        for line in drawing[0..end_of_stacks].lines() {
            for (index, c) in line.chars().enumerate() {
                // [S] [D]     [W]     [W]     [H] [Q]
                //  |   |       |
                // 0123012301230123...
                //  ^   ^       ^
                if index % 4 == 1 && c.is_alphabetic() {
                    stacks[index / 4].push_back(c)
                }
            }
        }
        // parse command
        let re = Regex::new(r"^move (?P<count>\d+) from (?P<start>\d+) to (?P<end>\d+)$").unwrap();
        for line in commands.lines() {
            if let Some(caps) = re.captures(line) {
                let count = caps["count"].parse::<usize>().unwrap();
                let start = caps["start"].parse::<usize>().unwrap();
                let end = caps["end"].parse::<usize>().unwrap();
                let mut to_move = Vec::new();
                for _ in 0..count {
                    to_move.push(stacks[start - 1].pop_front().unwrap());
                }
                for _ in 0..count {
                    stacks[end - 1].push_front(to_move.pop().unwrap());
                }
            }
        }
        stacks.iter().map(|stack| stack[0]).collect::<String>()
    }

    match part {
        Part::One => Solution::String(part1(input)),
        Part::Two => Solution::String(part2(input)),
    }
}

/// solves problem for day 6
fn day6(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> usize {
        let mut sliding_window: [char; 4] = dbg!(input.chars().take(4).collect::<Vec<char>>())
            .try_into()
            .unwrap();
        let mut end_of_marker = 0;
        for (index, c) in input.chars().skip(4).enumerate() {
            dbg!((&c, sliding_window));
            sliding_window[0] = c;
            sliding_window.rotate_left(1);
            if HashSet::from(sliding_window).len() == 4 {
                end_of_marker = index + 4;
                break;
            }
        }
        end_of_marker + 1 // problem is 1-indexed
    }
    fn part2(input: &str) -> usize {
        let mut sliding_window: [char; 14] = input
            .chars()
            .take(14)
            .collect::<Vec<char>>()
            .try_into()
            .unwrap();
        let mut end_of_marker = 0;
        for (index, c) in input.chars().skip(14).enumerate() {
            sliding_window[0] = c;
            sliding_window.rotate_left(1);
            if HashSet::from(sliding_window).len() == 14 {
                end_of_marker = index + 14;
                break;
            }
        }
        end_of_marker + 1 // problem is 1-indexed
    }
    match part {
        Part::One => Solution::Number(part1(input)),
        Part::Two => Solution::Number(part2(input)),
    }
}

/// passes problem input to solver for the given day
fn main() -> Result<(), Box<dyn Error>> {
    let days = [day1, day2, day3, day4, day5, day6];
    let today = 6;
    let prod_or_test = "prod";
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        let day = days[today - 1];
        let contents = fs::read_to_string(format!("./input/day{}.{}", today, prod_or_test))
            .expect("where's the input file? didn't find it at './input'");
        let part1 = day(&contents, Part::One);
        let part2 = day(&contents, Part::Two);
        Ok(println!("Solutions:\nPart 1: {}, Part 2: {}", part1, part2))
    } else {
        let day_index = args[1].parse::<usize>()?;
        let day = days[day_index - 1];
        let part = args[2].parse::<usize>()?.try_into()?;
        let contents = fs::read_to_string(format!("./input/day{}.{}", day_index, prod_or_test))
            .expect("where's the input file? didn't find it at './input'");
        let solution = day(&contents, part);
        Ok(println!("Solution: {}", solution))
    }
}
