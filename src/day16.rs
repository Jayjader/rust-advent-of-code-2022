use std::cmp::{Ordering, Reverse};
use std::collections::{BinaryHeap, HashMap, HashSet};

use regex::Regex;

use crate::{Part, Solution};

type Valve = String;
type FlowRate = u32;
type Nd = (Valve, FlowRate);

#[derive(PartialEq, Eq, Debug, Clone)]
enum Action {
    TurnOn(Valve),
    MoveTo(Valve),
}

type Tick = u32;

#[derive(PartialEq, Eq, Debug)]
struct State<const NUMBER_OF_ACTORS: usize> {
    total_flow_after_tick_30: FlowRate,
    current_tick: Reverse<Tick>,
    position: [Valve; NUMBER_OF_ACTORS],
    turned_on: HashSet<Valve>,
    actions: Vec<[Action; NUMBER_OF_ACTORS]>,
}

impl PartialOrd for State<1> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl PartialOrd for State<2> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for State<2> {
    fn cmp(&self, other: &Self) -> Ordering {
        match self
            .total_flow_after_tick_30
            .cmp(&other.total_flow_after_tick_30)
        {
            Ordering::Equal => match self.current_tick.cmp(&other.current_tick) {
                Ordering::Equal => self.turned_on.len().cmp(&other.turned_on.len()),
                others => others,
            },
            others => others,
        }
    }
}
impl Ord for State<1> {
    fn cmp(&self, other: &Self) -> Ordering {
        match self
            .total_flow_after_tick_30
            .cmp(&other.total_flow_after_tick_30)
        {
            Ordering::Equal => match self.current_tick.cmp(&other.current_tick) {
                Ordering::Equal => self.turned_on.len().cmp(&other.turned_on.len()),
                others => others,
            },
            others => others,
        }
    }
}

fn parse_valves_and_tunnels(
    input: &str,
) -> (HashMap<Valve, FlowRate>, HashMap<Valve, Vec<String>>) {
    let line_regex = Regex::new(
        r"Valve (?P<valve>[A-Z]{2}) has flow rate=(?P<flow_rate>\d+); tunnels? leads? to valves? (?P<tunnels_to>[A-Z]{2}(?:, [A-Z]{2})*)")
        .unwrap();
    let nodes: Vec<(Nd, Vec<String>)> = input
        .lines()
        .flat_map(|line| line_regex.captures(line))
        .map(|captures| {
            let rate = captures["flow_rate"].parse::<u32>().unwrap();
            let valve = &captures["valve"];
            let tunnels = captures["tunnels_to"]
                .split(", ")
                .map(String::from)
                .collect();
            ((String::from(valve), rate), tunnels)
        })
        .collect();
    let valve_rates: HashMap<Valve, FlowRate> =
        nodes
            .iter()
            .fold(HashMap::new(), |mut map, ((valve, rate), _)| {
                map.insert(valve.clone(), *rate);
                map
            });
    let edges = nodes.iter().fold(
        HashMap::with_capacity(nodes.len()),
        |mut edges, (current_node, tunnels)| {
            edges.insert(current_node.clone().0, tunnels.clone());
            edges
        },
    );
    (valve_rates, edges)
}

/// solves the problem for day 16
pub fn day16(input: &str, part: Part) -> Solution {
    fn part1(input: &str) -> u32 {
        let last_tick = 30;
        let (valve_rates, edges) = parse_valves_and_tunnels(input);

        let mut stack = BinaryHeap::<State<1>>::new();
        // current node <- AA
        // current tick <- 0
        stack.push(State {
            total_flow_after_tick_30: 0,
            current_tick: Reverse(0),
            position: [Valve::from("AA")],
            turned_on: Default::default(),
            actions: vec![[Action::MoveTo(Valve::from("AA"))]],
        });
        //
        let non_zero_count = valve_rates.iter().filter(|(_, rate)| **rate > 0).count();
        let mut greatest_total_flow_rate_found_so_far = 0;
        // for tick from 1 to 30:
        for tick in 1..=last_tick {
            println!(
                "\n===###=== stack at start of tick {}: (len: {}) ===###===",
                tick,
                stack.len(),
            );
            // for each previous state:
            let mut next_stack = BinaryHeap::new();
            while let Some(previous_state) = stack.pop() {
                greatest_total_flow_rate_found_so_far = previous_state
                    .total_flow_after_tick_30
                    .max(greatest_total_flow_rate_found_so_far);
                // skip exploring further actions if all valves are already on (the actions wouldn't bring new results)
                if previous_state.turned_on.len() == non_zero_count {
                    // keep this list of actions for future comparison with other lists of actions
                    next_stack.push(State {
                        current_tick: Reverse(tick),
                        ..previous_state
                    });
                    continue;
                }
                // if all remaining un-opened valves were magically opened this tick, could we do better than the current best list of actions?
                let unopened: HashSet<Valve> = valve_rates
                    .keys()
                    .cloned()
                    .collect::<HashSet<_>>()
                    .difference(&previous_state.turned_on)
                    .cloned()
                    .collect();
                if greatest_total_flow_rate_found_so_far
                    > (previous_state.total_flow_after_tick_30
                        + (last_tick - tick)
                            * unopened
                                .iter()
                                .map(|valve| valve_rates.get(valve).unwrap())
                                .cloned()
                                .sum::<u32>())
                {
                    // there's no way to get a better result by pursuing this line of action, so we just discard this list of actions for the next tick
                    continue;
                }
                // if flow rate > 0 and current node is off:
                let current_valve = &previous_state.position;
                let rate = valve_rates.get(&current_valve[0]).unwrap();
                if *rate > 0 && !previous_state.turned_on.contains(&current_valve[0]) {
                    // push [turn on current node at tick == [previous total flow] + [flow rate] * [ticks from tick until 30]] onto stack
                    let mut next_turned_on = previous_state.turned_on.clone();
                    next_turned_on.insert(current_valve[0].clone());
                    let mut next_actions = previous_state.actions.to_vec();
                    next_actions.push([Action::TurnOn(current_valve[0].clone())]);
                    next_stack.push(State {
                        total_flow_after_tick_30: previous_state.total_flow_after_tick_30
                            + (*rate * (last_tick - tick)),
                        current_tick: Reverse(tick),
                        position: current_valve.clone(),
                        turned_on: next_turned_on,
                        actions: next_actions,
                    })
                }
                // for node in [edges from current node]:
                for reachable_valve in edges
                    .get(&current_valve[0])
                    .unwrap()
                    .iter()
                    .filter(|valve| **valve != current_valve[0])
                {
                    // ignore actions that would make us loop back on our tracks without turning on any new valves
                    // we will end up turning them on later anyways after a non-minimal time spent moving
                    let last_turn_on_valve_index =
                        previous_state.actions.iter().enumerate().rev().find_map(
                            |(index, action)| match action[0] {
                                Action::TurnOn(_) => Some(index),
                                Action::MoveTo(_) => None,
                            },
                        );
                    let slice_start = last_turn_on_valve_index
                        .map(|index| index - 1 /* MoveTo action is 1 before the TurnOn that can be detected in the following if statement */)
                        .unwrap_or(0);
                    if !previous_state.actions[slice_start..]
                        .contains(&[Action::MoveTo(String::from(reachable_valve))])
                    {
                        // push [move to node at current tick == [previous total flow]] onto stack
                        let mut next_actions = previous_state.actions.to_vec();
                        next_actions.push([Action::MoveTo(reachable_valve.clone())]);
                        next_stack.push(State {
                            total_flow_after_tick_30: previous_state.total_flow_after_tick_30,
                            current_tick: Reverse(tick),
                            position: [reachable_valve.clone()],
                            turned_on: previous_state.turned_on.clone(),
                            actions: next_actions,
                        })
                    }
                }
            }
            stack = next_stack;
        }
        stack.pop().unwrap().total_flow_after_tick_30
    }

    fn part2(input: &str) -> u32 {
        let last_tick = 26;
        let (valve_rates, edges) = parse_valves_and_tunnels(input);

        let mut stack = BinaryHeap::<State<2>>::new();
        // current node <- AA
        // current tick <- 0
        stack.push(State {
            total_flow_after_tick_30: 0,
            current_tick: Reverse(0),
            position: [Valve::from("AA"), Valve::from("AA")],
            turned_on: Default::default(),
            actions: vec![[
                Action::MoveTo(Valve::from("AA")),
                Action::MoveTo(Valve::from("AA")),
            ]],
        });
        //
        let non_zero_count = valve_rates.iter().filter(|(_, rate)| **rate > 0).count();
        let mut greatest_total_flow_rate_found_so_far = 0;
        // for tick from 1 to 26:
        for tick in 1..=last_tick {
            println!(
                "\n===###=== stack at start of tick {}: (len: {}) ===###===",
                tick,
                stack.len(),
            );
            println!("{:?}", &stack);
            // for each previous state:
            let mut next_stack = BinaryHeap::new();
            while let Some(previous_state) = stack.pop() {
                greatest_total_flow_rate_found_so_far = previous_state
                    .total_flow_after_tick_30
                    .max(greatest_total_flow_rate_found_so_far);
                // skip exploring further actions if all valves are already on (the actions wouldn't bring new results)
                if previous_state.turned_on.len() == non_zero_count {
                    // keep this list of actions for future comparison with other lists of actions
                    next_stack.push(State {
                        current_tick: Reverse(tick),
                        ..previous_state
                    });
                    continue;
                }
                // if all remaining un-opened valves were magically opened this tick, could we do better than the current best list of actions?
                let unopened: HashSet<Valve> = valve_rates
                    .keys()
                    .cloned()
                    .collect::<HashSet<_>>()
                    .difference(&previous_state.turned_on)
                    .cloned()
                    .collect();
                if greatest_total_flow_rate_found_so_far
                    > (previous_state.total_flow_after_tick_30
                        + (last_tick - tick)
                            * unopened
                                .iter()
                                .map(|valve| valve_rates.get(valve).unwrap())
                                .cloned()
                                .sum::<u32>())
                {
                    // there's no way to get a better result by pursuing this line of action, so we just discard this list of actions for the next tick
                    continue;
                }

                let mut next_action = [
                    Action::TurnOn("AA".to_string()),
                    Action::TurnOn("AA".to_string()),
                ];
                let mut next_state = State {
                    total_flow_after_tick_30: previous_state.total_flow_after_tick_30,
                    current_tick: Reverse(tick),
                    position: [Valve::from("AA"), Valve::from("AA")],
                    turned_on: previous_state.turned_on.clone(),
                    actions: previous_state.actions.clone(),
                };
                for (actor_index, current_valve) in previous_state.position.iter().enumerate() {
                    // if flow rate > 0 and current node is off:
                    let rate = valve_rates.get(current_valve).unwrap();
                    if *rate > 0 && !next_state.turned_on.contains(current_valve) {
                        // push [turn on current node at tick == [previous total flow] + [flow rate] * [ticks from tick until 26]] onto stack
                        next_action[actor_index] = Action::TurnOn(current_valve.to_owned());

                        next_state.total_flow_after_tick_30 =
                            previous_state.total_flow_after_tick_30 + (*rate * (last_tick - tick));
                        next_state.position[actor_index] = current_valve.to_owned();
                        next_state.turned_on.insert(current_valve.to_owned());
                    }

                    // for node in [edges from current node]:
                    for reachable_valve in edges
                        .get(current_valve)
                        .unwrap()
                        .iter()
                        .filter(|valve| *valve != current_valve)
                    {
                        // ignore actions that would make us loop back on our tracks without turning on any new valves
                        // we will end up turning them on later anyways after a non-minimal time spent moving
                        let last_turn_on_valve_index =
                            previous_state.actions.iter().enumerate().rev().find_map(
                                |(index, action)| match action {
                                    [_, Action::TurnOn(_)] => Some(index),
                                    [Action::TurnOn(_), _] => Some(index),
                                    [_, _] => None,
                                },
                            );
                        let slice_start = last_turn_on_valve_index
                        .map(|index| index - 1 /* MoveTo action is 1 before the TurnOn that can be detected in the following if statement */)
                        .unwrap_or(0);
                        if !previous_state.actions[slice_start..]
                            .iter()
                            .any(|action_array| {
                                action_array[actor_index]
                                    == Action::MoveTo(String::from(reachable_valve))
                            })
                        // .contains(&Action::MoveTo(String::from(reachable_valve)))
                        {
                            // push [move to node at current tick == [previous total flow]] onto stack
                            // let mut next_actions: Vec<Action> = previous_state.actions.to_vec();
                            next_action[actor_index] = Action::MoveTo(reachable_valve.to_owned());
                            // next_actions.push(Action::MoveTo(reachable_valve.clone()));
                            next_state.position[actor_index] = reachable_valve.to_owned();
                            // next_stack.push(State {
                            // total_flow_after_tick_30: previous_state.total_flow_after_tick_30,
                            // current_tick: Reverse(tick),
                            // position: reachable_valve.clone(),
                            // turned_on: previous_state.turned_on.clone(),
                            // actions: next_actions,
                            // })
                        }
                    }
                }
                // next_stack.push(State {
                //     total_flow_after_tick_30: previous_state.total_flow_after_tick_30
                //         + (*rate * (30 - tick)),
                //     current_tick: Reverse(tick),
                //     position: current_valve.clone(),
                //     turned_on: next_turned_on,
                //     actions: next_actions,
                // })
                next_state.actions.push(next_action);
                next_stack.push(next_state);
            }
            stack = next_stack;
        }
        stack.pop().unwrap().total_flow_after_tick_30
    }

    Solution::U32(match part {
        Part::One => part1(input),
        Part::Two => part2(input),
    })
}
