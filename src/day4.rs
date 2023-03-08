use crate::{Part, Solution};

/// solves problem for day 4
pub fn day4(input: &str, part: Part) -> Solution {
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
        Part::One => Solution::USize(part1(input)),
        Part::Two => Solution::USize(part2(input)),
    }
}
