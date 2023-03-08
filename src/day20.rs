use crate::{Part, Solution};

fn mix(original: &Vec<i64>, current: Vec<(usize, i64)>) -> Vec<(usize, i64)> {
    let mut new = current.to_vec();
    let length = original.len();
    for (original_index, &number) in original.iter().enumerate() {
        if number == 0 {
            continue;
        }
        let current_index = new
            .iter()
            .enumerate()
            .find(|(_current_index, (index, _number))| *index == original_index)
            .unwrap()
            .0;
        let pair = new.remove(current_index);
        let new_index = (current_index as i64 + number).rem_euclid(length as i64 - 1) as usize;
        new.insert(new_index, pair);
    }
    new
}

fn sum_grove_coords(length: usize, new: Vec<(usize, i64)>) -> isize {
    let position_zero = new
        .iter()
        .enumerate()
        .find(|(_index, (_, number))| *number == 0)
        .unwrap()
        .0;
    let grove_coords_indices = [
        (position_zero + 1_000).rem_euclid(length),
        (position_zero + 2_000).rem_euclid(length),
        (position_zero + 3_000).rem_euclid(length),
    ];
    grove_coords_indices
        .iter()
        .map(|&index| new[index].1)
        .sum::<i64>() as isize
}

/// solves the problem for day 20
pub fn day20(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> isize {
        let original: Vec<_> = input.lines().flat_map(str::parse::<i64>).collect();
        let new = mix(
            &original,
            original.iter().cloned().enumerate().collect::<Vec<_>>(),
        );
        sum_grove_coords(original.len(), new)
    }
    fn part2(input: &str) -> isize {
        const DECRYPTION_KEY: i64 = 811589153;
        let original: Vec<_> = input
            .lines()
            .flat_map(str::parse::<i64>)
            .map(|n| n * DECRYPTION_KEY)
            .collect();
        let mut new = original.iter().cloned().enumerate().collect();
        for _ in 0..10 {
            new = mix(&original, new);
        }
        sum_grove_coords(original.len(), new)
    }

    Solution::INumber(match part {
        Part::One => part1(input),
        Part::Two => part2(input),
    })
}
