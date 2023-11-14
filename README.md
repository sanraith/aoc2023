# aoc2023

Solutions for Advent of Code 2023 in Scala.

## TODO

- [x] Scaffold framework
  - [x] Generate index
  - [x] Generate solution file
  - [x] Generate test file
  - [x] Load title from web
  - [x] Add caching for loading
  - [x] Load test data from web
  - [x] Load real data from web
  - [x] Scaffold only inputs
- [x] Test framework
  - [x] Use test data starting with line break
  - [x] Use real data
- [x] Run framework
  - [x] Run solutions
  - [x] Use real data
  - [x] Show time
  - [x] Show multiline results in new line
  - [x] Show progress
- [ ] Requirements
- [ ] Command descriptions

## Requirements

TODO

## Commands

Use `sbt <command>`, or start `sbt` and enter one of the commands:

- `run` / `run all`
- `run last`
- `run 12` / `run day 12` / `run days 1 2 3`
- `run scaffold`
- `run scaffold 12` / `run scaffold day 12` / `run scaffold days 1 2 3`
- `run scaffold inputs`
- `run scaffold input 12` / `run scaffold inputs 1 2 3`
- `test`

## Advent of Code Automation

This repository does follow the automation guidelines on the /r/adventofcode [community wiki](https://www.reddit.com/r/adventofcode/wiki/faqs/automation). Specifically:

- Outbound calls are only triggered manually, by `sbt> run scaffold [...]` -> `Main.scaffold()`
- All successful web requests are cached locally in `.cache/` by `WebClient.requestCached()`
- If you suspect your input is corrupted, you can manually request a fresh copy by
  1. deleting the cached input from `.cache/`
  2. `sbt> run scaffold inputs`
- The User-Agent header in `WebClient.request()` is set to me since I maintain this repository.
