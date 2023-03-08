extern crate regex;

use core::convert::TryFrom;
use core::convert::TryInto;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::{env, fs};

mod day1;
mod day10;
mod day11;
mod day12;
mod day13;
mod day14;
mod day15;
mod day16;
mod day2;
mod day20;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;

pub enum Part {
    One,
    Two,
}

#[derive(Debug, Clone)]
pub struct PartParseError(usize);

impl TryFrom<usize> for Part {
    type Error = PartParseError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Part::One),
            2 => Ok(Part::Two),
            _ => Err(PartParseError(value)),
        }
    }
}

impl Display for PartParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "couldn't parse problem part '{}'", self.0)
    }
}

impl Error for PartParseError {}

/// Solution can be any exactly one of these for any given problem part
#[derive(Debug)]
pub enum Solution {
    USize(usize),
    U32(u32),
    U64(u64),
    INumber(isize),
    String(String),
}
impl Display for Solution {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Solution::USize(n) => {
                write!(f, "{}", n)
            }
            Solution::U32(n) => {
                write!(f, "{}", n)
            }
            Solution::U64(n) => {
                write!(f, "{}", n)
            }
            Solution::INumber(n) => {
                write!(f, "{}", n)
            }
            Solution::String(s) => {
                write!(f, "{}", s)
            }
        }
    }
}

fn day17(_input: &str, _part: Part) -> Solution {
    unimplemented!();
}
fn day18(_input: &str, _part: Part) -> Solution {
    unimplemented!();
}
fn day19(_input: &str, _part: Part) -> Solution {
    unimplemented!();
}

/// passes problem input to solver for the given day
fn main() -> Result<(), Box<dyn Error>> {
    //todo: extract visualization generation into separate binaries and/or a command line argument flag
    let days = [
        day1::day1,
        day2::day2,
        day3::day3,
        day4::day4,
        day5::day5,
        day6::day6,
        day7::day7,
        day8::day8,
        day9::day9,
        day10::day10,
        day11::day11,
        day12::day12,
        day13::day13,
        day14::day14,
        day15::day15,
        day16::day16,
        day17,
        day18,
        day19,
        day20::day20,
    ];
    let today = 12;
    let prod_or_test = "prod";
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        let day = days[today - 1];
        let input_path = format!("./input/day{}.{}", today, prod_or_test);
        let contents = fs::read_to_string(&input_path).unwrap_or_else(|_| {
            panic!(
                "where's the input file? didn't find it at '{}'",
                &input_path
            )
        });
        let part1 = day(&contents, Part::One);
        let part2 = day(&contents, Part::Two);
        Ok(println!(
            "Solutions:\nPart 1:\n{}\nPart 2:\n{}",
            part1, part2
        ))
    } else {
        let day_index = args[1].parse::<usize>()?;
        let day = days[day_index - 1];
        let part = args[2].parse::<usize>()?.try_into()?;
        let input_path = format!("./input/day{}.{}", today, prod_or_test);
        let contents = fs::read_to_string(&input_path).unwrap_or_else(|_| {
            panic!(
                "where's the input file? didn't find it at '{}'",
                &input_path
            )
        });
        let solution = day(&contents, part);
        Ok(println!("Solution:\n{}", solution))
    }
}
