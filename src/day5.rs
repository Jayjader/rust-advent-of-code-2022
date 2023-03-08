use std::collections::VecDeque;

use regex::Regex;

use crate::{Part, Solution};

/// solves problem for day 5
pub fn day5(input: &str, part: Part) -> Solution {
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
