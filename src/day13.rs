use std::cmp::Ordering;

use crate::{Part, Solution};

fn tokenize(input: &str) -> Vec<String> {
    input
        .replace('[', " [ ")
        .replace(']', " ] ")
        .replace(',', " , ")
        .split_whitespace()
        .map(str::to_owned)
        .collect()
}

#[derive(Debug, Clone)]
enum PacketData {
    List(Vec<PacketData>),
    Int(usize),
}

fn simple_rec<'a>(
    mut result: Vec<PacketData>,
    tokens: &'a [&'a str],
) -> (Vec<PacketData>, Option<&'a [&'a str]>) {
    match tokens.split_first() {
        None => (result, None),
        Some((token, rest)) => match token {
            &"," => simple_rec(result, rest),
            &"]" => (result, Some(rest)),
            &"[" => {
                let new_list = Vec::new();

                let (updated_new_list, rest) = simple_rec(new_list, rest);
                result.push(PacketData::List(updated_new_list));
                simple_rec(result, rest.unwrap())
            }
            int if int.parse::<usize>().is_ok() => {
                result.push(PacketData::Int(int.parse().unwrap()));
                simple_rec(result, rest)
            }
            _ => unreachable!(),
        },
    }
}

fn in_place_ordering(result: Ordering, p1: &PacketData, p2: &PacketData) -> Ordering {
    match result {
        Ordering::Greater => Ordering::Greater,
        Ordering::Less => Ordering::Less,
        Ordering::Equal => match (p1, p2) {
            (PacketData::Int(self_int), PacketData::Int(other_int)) => self_int.cmp(other_int),
            (PacketData::Int(self_int), PacketData::List(_)) => in_place_ordering(
                result,
                &PacketData::List(vec![PacketData::Int(*self_int)]),
                p2,
            ),
            (PacketData::List(_), PacketData::Int(other_int)) => in_place_ordering(
                result,
                p1,
                &PacketData::List(vec![PacketData::Int(*other_int)]),
            ),
            (PacketData::List(self_list), PacketData::List(other_list)) => {
                match (self_list.first(), other_list.first()) {
                    (None, None) => result,
                    (None, Some(_)) => Ordering::Less,
                    (Some(_), None) => Ordering::Greater,
                    (Some(self_first), Some(other_first)) => {
                        match in_place_ordering(result, self_first, other_first) {
                            Ordering::Less => Ordering::Less,
                            Ordering::Greater => Ordering::Greater,
                            Ordering::Equal => in_place_ordering(
                                Ordering::Equal,
                                &PacketData::List(self_list.iter().skip(1).cloned().collect()),
                                &PacketData::List(other_list.iter().skip(1).cloned().collect()),
                            ),
                        }
                    }
                }
            }
        },
    }
}

fn ordered(p1: &PacketData, p2: &PacketData) -> bool {
    in_place_ordering(Ordering::Equal, p1, p2) == Ordering::Less
}

/// solves the problem for day13
pub fn day13(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> usize {
        input
            .split_terminator("\n\n")
            .enumerate()
            .flat_map(|(index, pair)| {
                let (first, second) = pair.split_once('\n').expect("need newline between pair");
                let tokenized_first = tokenize(first);
                let first = tokenized_first
                    .iter()
                    .skip(1)
                    .map(String::as_str)
                    .collect::<Vec<_>>();
                let first_vec = Vec::new();
                let (first, _) = simple_rec(first_vec, first.as_slice());
                let tokenized_second = tokenize(second);
                let second = tokenized_second
                    .iter()
                    .skip(1)
                    .map(String::as_str)
                    .collect::<Vec<_>>();
                let second_vec = Vec::new();
                let (second, _) = simple_rec(second_vec, second.as_slice());
                let first = PacketData::List(first);
                let second = PacketData::List(second);

                if ordered(&first, &second) {
                    Some(index + 1)
                } else {
                    None
                }
            })
            .sum()
    }

    fn part2(input: &str) -> usize {
        let mut packets: Vec<_> = input
            .split_whitespace()
            .map(|packet_str| {
                let vec = Vec::new();
                let tokens = tokenize(packet_str);
                let tokens: Vec<_> = tokens.iter().skip(1).map(String::as_str).collect();
                PacketData::List(simple_rec(vec, tokens.as_slice()).0)
            })
            .collect();
        let v_two = vec![PacketData::List(vec![PacketData::Int(2)])];
        packets.push(PacketData::List(v_two.clone()));
        let p_compare_two = PacketData::List(v_two);
        let v_six = vec![PacketData::List(vec![PacketData::Int(6)])];
        packets.push(PacketData::List(v_six.clone()));
        let p_compare_six = PacketData::List(v_six);
        packets.sort_by(|p1, p2| in_place_ordering(Ordering::Equal, p1, p2));
        let (two_index, _) = packets
            .iter()
            .enumerate()
            .find(|p| Ordering::Equal == in_place_ordering(Ordering::Equal, p.1, &p_compare_two))
            .unwrap();
        let (six_index, _) = packets
            .iter()
            .enumerate()
            .find(|p| Ordering::Equal == in_place_ordering(Ordering::Equal, p.1, &p_compare_six))
            .unwrap();
        (two_index + 1) * (six_index + 1)
    }
    match part {
        Part::One => Solution::USize(part1(input)),
        Part::Two => Solution::USize(part2(input)),
    }
}
