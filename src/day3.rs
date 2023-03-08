use std::collections::HashSet;

use crate::{Part, Solution};

/// common sub-problem for both parts of day 3
fn priority_for_char(shared_char: &char) -> usize {
    (*shared_char as u32 as usize)
        - (if shared_char.is_uppercase() {
            65 - 27 // 65 is 'A' which has priority 26
        } else {
            97 - 1 // 97 is 'a' which has priority 1
        })
}

/// solve problem for day 3
pub fn day3(input: &str, part: Part) -> Solution {
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
        Part::One => Solution::USize(part1(input)),
        Part::Two => Solution::USize(part2(input)),
    }
}
