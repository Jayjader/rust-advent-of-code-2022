use std::collections::HashSet;

use crate::{Part, Solution};

/// finds the first window of length N over bytes in input str slice that contains no duplicates
/// return the position (in the overall input slice) of the last byte in the window found
/// common sub-problem for both parts
fn slide_window_until_unique<const N: usize>(input: &str) -> usize {
    input
        .as_bytes()
        .windows(N)
        .position(|window| HashSet::<&u8>::from_iter(window).len() == N)
        .unwrap()
        + N
}

/// solves problem for day 6
pub fn day6(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> usize {
        slide_window_until_unique::<4>(input)
    }

    fn part2(input: &str) -> usize {
        slide_window_until_unique::<14>(input)
    }
    match part {
        Part::One => Solution::USize(part1(input)),
        Part::Two => Solution::USize(part2(input)),
    }
}
