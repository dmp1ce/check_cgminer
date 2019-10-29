# Changelog for check_cgminer

## 0.7.2.0

- Fix divide by zero error when a miner hash 0 hash rate

## 0.7.1.0

- Correct workmode for S17 miner

## 0.7.0.0 - Profitability improvements

- Show profitability in OK description
- Dynamically lookup block reward and mining fee reward
- Allow override of block reward and mining reward in CLI options
- Add pool fee factor to profitability calculation with CLI option

## 0.6.2.0

- Fix duplicate profitability outputs
- Add thresholds for profitability performance data

## 0.6.1.0

- Add profitability thresholds
- Change profitability unit to USD/day
- Add profitability for S17Pro turbo mode

## 0.6.0.0

- Add profitability check

## 0.5.4.0

- Add support for Antminer S9k and Antminer S9 SE

## 0.5.3.0

- Add "work mode" performance data for S17 miners

## 0.5.2.0

- Adjust max frequency threshold to 5000

## 0.5.1.0

- Fix voltage and frequency bounds in performance data

## 0.5.0.0

- Add support for frequency and voltage upper thresholds for S9 devices

## 0.4.1.0

- Add support for DR5 and Z9-mini devices
- Add customize-able maximum hash value
- Add customize-able hashunit string
- Remove space characters from performance data strings

## 0.4.0.0

- Add support for Whatsminer devices

## 0.3.3.0

- Add support for S15 miners

## 0.3.2.0

- Add support for more S17 miner types
- Improve error messages

## 0.3.1.0

- Add support for S17 miners
- Improve string wording

## 0.3.0.0

- Add upper threshold for fan speeds

## 0.2.1.0

- Parse hash rate empty string as zero

## 0.2.0.0

- Add support for fan speed monitoring
- Add support for hashrate monitoring
- Add version switch

## 0.1.0.0

- Add support for temperature monitoring
