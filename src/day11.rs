use std::collections::VecDeque;
use std::str::FromStr;

use crate::{Part, Solution};

#[derive(Debug, Clone, Copy)]
struct Test {
    divisor: u64,
    true_: usize,
    false_: usize,
}
impl FromStr for Test {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (divisor, rest) = s
            .split_once("Test: divisible by ")
            .unwrap()
            .1
            .split_once('\n')
            .unwrap();
        let divisor = divisor.parse().unwrap();
        let (true_, false_) = rest.split_once('\n').unwrap();
        let true_ = true_
            .split_once("If true: throw to monkey ")
            .unwrap()
            .1
            .parse()
            .unwrap();
        let false_ = false_
            .split_once("If false: throw to monkey ")
            .unwrap()
            .1
            .parse()
            .unwrap();

        Ok(Test {
            divisor,
            true_,
            false_,
        })
    }
}
#[derive(Debug, Clone, Copy)]
enum Arg {
    Number(u64),
    Old,
}
impl FromStr for Arg {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "old" => Ok(Arg::Old),
            _ => Ok(Arg::Number(s.parse().unwrap())),
        }
    }
}
#[derive(Debug, Clone, Copy)]
enum Operation {
    Add(Arg),
    Mult(Arg),
}
impl FromStr for Operation {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let data = s.split_once("Operation: new = old ").unwrap().1;
        let (op, arg) = data.split_once(' ').unwrap();
        let arg = arg.parse().unwrap();
        match op {
            "+" => Ok(Operation::Add(arg)),
            "*" => Ok(Operation::Mult(arg)),
            _ => Err(()),
        }
    }
}
#[derive(Debug, Clone)]
struct Monkey {
    inspections: usize,
    items: Vec<u64>,
    operation: Operation,
    test: Test,
}
impl FromStr for Monkey {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (_header, rest) = s.split_once('\n').unwrap();
        let (items, rest) = rest.split_once('\n').unwrap();
        let items = items
            .split_once("Starting items: ")
            .unwrap()
            .1
            .split(", ")
            .flat_map(str::parse)
            .collect();
        let (operation, test) = rest.split_once('\n').unwrap();
        let operation = operation.parse().unwrap();
        let test = test.trim().parse().unwrap();
        Ok(Monkey {
            inspections: 0,
            items,
            operation,
            test,
        })
    }
}

/// core of solution for part 1
fn round_recursively(
    mut has_inspected_and_thrown: VecDeque<Monkey>,
    mut has_not_inspected_nor_thrown: VecDeque<Monkey>,
) -> (VecDeque<Monkey>, VecDeque<Monkey>) {
    let next_monkey = has_not_inspected_nor_thrown.pop_front();
    match next_monkey {
        None => (has_inspected_and_thrown, has_not_inspected_nor_thrown),
        Some(monkey) => {
            let mut next_monkey = Monkey { ..monkey };
            next_monkey.inspections += next_monkey.items.len();
            for item_worry in (next_monkey.items).iter() {
                println!(
                    "  Monkey inspects an item with a worry level of {}.",
                    item_worry
                );
                // -> apply operation
                print!("    Worry level "); // is multiplied by 19 to 1501.");
                let item_worry = match &monkey.operation {
                    Operation::Add(arg) => {
                        print!("increases by ");
                        match arg {
                            Arg::Number(n) => {
                                print!("{}", n);
                                item_worry + n
                            }
                            Arg::Old => {
                                print!("{}", item_worry);
                                item_worry + item_worry
                            }
                        }
                    }
                    Operation::Mult(arg) => {
                        print!("is multiplied by ");
                        match arg {
                            Arg::Number(n) => {
                                print!("{}", n);
                                item_worry * n
                            }
                            Arg::Old => {
                                print!("{}", item_worry);
                                item_worry * item_worry
                            }
                        }
                    }
                };
                println!(" to {}", item_worry);
                // -> reduce by 3, rounded down
                let item_worry = item_worry / 3;
                println!(
                    "    Monkey gets bored with item. Worry level is divided by 3 to {}.",
                    item_worry
                );
                print!("    Current worry level is ");
                let receiver_index = if item_worry % monkey.test.divisor == 0 {
                    monkey.test.true_
                } else {
                    print!("not");
                    monkey.test.false_
                };
                println!(" divisible by {}.", monkey.test.divisor);
                println!(
                    "    Item with worry level {} is thrown to monkey {}",
                    item_worry, receiver_index
                );
                if receiver_index >= has_inspected_and_thrown.len() {
                    has_not_inspected_nor_thrown
                        [receiver_index - has_inspected_and_thrown.len() - 1]
                        .items
                        .push(item_worry);
                } else {
                    has_inspected_and_thrown[receiver_index]
                        .items
                        .push(item_worry);
                }
            }
            next_monkey.items.clear();

            has_inspected_and_thrown.push_back(next_monkey);
            round_recursively(has_inspected_and_thrown, has_not_inspected_nor_thrown)
        }
    }
}

/// core of solution for part 2
fn round_recursively_ridiculous_levels(
    divisor_product: u64,
    mut has_inspected_and_thrown: VecDeque<Monkey>,
    mut has_not_inspected_nor_thrown: VecDeque<Monkey>,
) -> (VecDeque<Monkey>, VecDeque<Monkey>) {
    let next_monkey = has_not_inspected_nor_thrown.pop_front();
    match next_monkey {
        None => (has_inspected_and_thrown, has_not_inspected_nor_thrown),
        Some(monkey) => {
            let mut next_monkey = Monkey { ..monkey };
            next_monkey.inspections += next_monkey.items.len();
            for item_worry in (next_monkey.items).iter() {
                let next_item_worry = match &monkey.operation {
                    Operation::Add(arg) => {
                        item_worry
                            + match arg {
                                Arg::Number(n) => n,
                                Arg::Old => item_worry,
                            }
                    }
                    Operation::Mult(arg) => {
                        (item_worry
                            * (match arg {
                                Arg::Number(n) => n,
                                Arg::Old => item_worry,
                            } % divisor_product))
                            % divisor_product
                    }
                };
                let receiver_index = if next_item_worry % monkey.test.divisor == 0 {
                    monkey.test.true_
                } else {
                    monkey.test.false_
                };
                if receiver_index >= has_inspected_and_thrown.len() {
                    has_not_inspected_nor_thrown
                        [receiver_index - has_inspected_and_thrown.len() - 1]
                        .items
                        .push(next_item_worry);
                } else {
                    has_inspected_and_thrown[receiver_index]
                        .items
                        .push(next_item_worry);
                }
            }
            next_monkey.items.clear();

            has_inspected_and_thrown.push_back(next_monkey);
            round_recursively_ridiculous_levels(
                divisor_product,
                has_inspected_and_thrown,
                has_not_inspected_nor_thrown,
            )
        }
    }
}

/// solves the problem for day 11
pub fn day11(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> usize {
        let mut monkeys = input
            .split_terminator("\n\n")
            .flat_map(|monkey_input| monkey_input.parse::<Monkey>())
            .collect::<VecDeque<_>>();
        for _ in 1..21 {
            monkeys = round_recursively(VecDeque::with_capacity(monkeys.len()), monkeys).0;
        }
        let mut monkeys = monkeys.into_iter().collect::<Vec<_>>();
        monkeys.sort_by(|m0, m1| m1.inspections.cmp(&m0.inspections));
        monkeys
            .iter()
            .take(2)
            .fold(1, |product, m| m.inspections * product)
    }

    fn part2(input: &str) -> usize {
        let mut monkeys = input
            .split_terminator("\n\n")
            .flat_map(|monkey_input| monkey_input.parse::<Monkey>())
            .collect::<VecDeque<_>>();
        let divisor_product: u64 = monkeys
            .iter()
            .map(|m| {
                // print!(" {} ", m.test.divisor);
                m.test.divisor
            })
            .product();
        // println!(" * = {} ", divisor_product);
        for i in 1..10_001 {
            monkeys = round_recursively_ridiculous_levels(
                divisor_product,
                VecDeque::with_capacity(monkeys.len()),
                monkeys,
            )
            .0;
            if [
                1, 20, 1_000, 2_000, 3_000, 4_000, 5_000, 6_000, 7_000, 9_000, 10_000,
            ]
            .contains(&i)
            {
                println!("\n== After round {} ==", i);
                for (i, m) in monkeys.iter().enumerate() {
                    println!("Monkey {} inspected items {} times", i, m.inspections);
                }
            }
            // dbg!(&monkeys);
        }
        // dbg!(&monkeys);
        let mut monkeys = monkeys.into_iter().collect::<Vec<_>>();
        monkeys.sort_by(|m0, m1| m1.inspections.cmp(&m0.inspections));
        monkeys
            .iter()
            .take(2)
            .fold(1, |product, m| m.inspections * product)
    }
    match part {
        Part::One => Solution::USize(part1(input)),
        Part::Two => Solution::USize(part2(input)),
    }
}
