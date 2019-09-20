# check_cgminer

[![CircleCI](https://circleci.com/gh/dmp1ce/check_cgminer.svg?style=svg)](https://circleci.com/gh/dmp1ce/check_cgminer)

Monitoring (Nagios) plugin for checking cgminer miner devices statistics from the cgminer API.

Tested devices:

- DR5 (missing power consumption default)
- S9
- S9k (missing power consumption default)
- S9se
- S15
- S17
- Z9-mini (missing power consumption default)
- Whatsminer (missing power consumption default)

Devices missing power consumption defaults will not calculate profitability unless `--device_power` is used.

## Help

```
check_cgminer --help
check_cgminer - Nagios monitoring plugin for cgminer API

Usage: check_cgminer [-v|--version] [-H|--host HOST] [-P|--port PORT]
                     [-t|--temp_warn NUMBER] [-T|--temp_crit NUMBER]
                     [-r|--hash_warn NUMBER] [-R|--hash_crit NUMBER]
                     [--hash_maximum NUMBER] [--hashunit STRING]
                     [-f|--fan_low_warn NUMBER] [-F|--fan_low_crit NUMBER]
                     [-n|--fan_high_warn NUMBER] [-N|--fan_high_crit NUMBER]
                     [--volt_high_warn NUMBER] [--volt_high_crit NUMBER]
                     [--freq_high_warn NUMBER] [--freq_high_crit NUMBER]
                     [--device_power NUMBER] [--electric_rate NUMBER]
                     [--prof_warn NUMBER] [--prof_crit NUMBER]
                     [--block_reward NUMBER] [--mining_fee_reward NUMBER]
                     [--pool_fee NUMBER]
  Return Nagios formatted string based on cgminer API returned values

Available options:
  -h,--help                Show this help text
  -v,--version             Show version information
  -H,--host HOST           Hostname of cgminer API (default: "127.0.0.1")
  -P,--port PORT           Port of cgminer API (default: "4028")
  -t,--temp_warn NUMBER    Warning temperature threshold in
                           Celsius (default: 90.0)
  -T,--temp_crit NUMBER    Critical temperature threshold in
                           Celsius (default: 100.0)
  -r,--hash_warn NUMBER    Warning hash rate threshold (default: 4000.0)
  -R,--hash_crit NUMBER    Critical hash rate threshold (default: 3000.0)
  --hash_maximum NUMBER    Maximum Hashrate (Used with performance
                           data) (default: 10000.0)
  --hashunit STRING        Hashing unit of measure (default: "Ghs")
  -f,--fan_low_warn NUMBER Warning low fan speed threshold in
                           RPMs (default: 999.0)
  -F,--fan_low_crit NUMBER Critical low fan speed threshold in
                           RPMs (default: 500.0)
  -n,--fan_high_warn NUMBER
                           Warning high fan speed threshold in
                           RPMs (default: 9000.0)
  -N,--fan_high_crit NUMBER
                           Critical high fan speed threshold in
                           RPMs (default: 10000.0)
  --volt_high_warn NUMBER  Warning high voltage threshold in Volts (Only
                           supported for S9 miners) (default: 20.0)
  --volt_high_crit NUMBER  Critical high voltage threshold in Volts (Only
                           supported for S9 miners) (default: 20.0)
  --freq_high_warn NUMBER  Warning high frequency threshold in Mhz (Only
                           supported for S9 miners) (default: 5000.0)
  --freq_high_crit NUMBER  Critical high frequency threshold in Mhz (Only
                           supported for S9 miners) (default: 5000.0)
  --device_power NUMBER    Override estimated device power consumption in Watt
  --electric_rate NUMBER   Default electricity rate in USD/kWh (default: 0.1188)
  --prof_warn NUMBER       Warning profitability threshold in
                           USD/day (default: 0.25)
  --prof_crit NUMBER       Critical profitability threshold in
                           USD/day (default: 0.0)
  --block_reward NUMBER    Override the block reward (default: API lookup)
  --mining_fee_reward NUMBER
                           Override the mining fee reward (default: API lookup)
  --pool_fee NUMBER        Pool fee percentage. Ex: 0.01 = 1% (default: 0.0)
```

## Usage

```
$ check_cgminer -H 10.0.0.163 --pool_fee "0.02"
OK: Profitability is 4.9064 USD/day, Max temp: 80.0 C, Min hashrate: 18647.21 Ghs, Min fanspeed: 3240.0 RPM | fan4=4320.0;;;0.0;20000.0 fan3=4320.0;;;0.0;20000.0 fan2=3240.0;;;0.0;20000.0 fan1=3240.0;;;0.0;20000.0 chain_rate3=18779.15;4000.0;3000.0;0.0;10000.0 chain_rate2=18647.21;4000.0;3000.0;0.0;10000.0 chain_rate1=19284.91;4000.0;3000.0;0.0;10000.0 temp3_3=76.0;90.0;100.0;20.0;120.0 temp3_2=80.0;90.0;100.0;20.0;120.0 temp3_1=78.0;90.0;100.0;20.0;120.0 temp2_3=63.0;90.0;100.0;20.0;120.0 temp2_2=64.0;90.0;100.0;20.0;120.0 temp2_1=65.0;90.0;100.0;20.0;120.0 temp3=58.0;90.0;100.0;20.0;120.0 temp2=61.0;90.0;100.0;20.0;120.0 temp1=61.0;90.0;100.0;20.0;120.0 profitability=4.9063987416773385;0.25;0.0;; WorkMode=2.0;;;0.0;2.0
```

## Profitability

`check_cgminer` will try to automatically detect the miners profitability by getting several factors.

- Power consumption (Attempts to auto detect miner and use hard coded wattage values by default)
- Electricity rate (Uses USA national average by default)
- Bitcoin price (Collected from https://apiv2.bitcoinaverage.com/)
- Difficulty (Collected from https://api-r.bitcoinchain.com/)
- Block reward (Collected from https://api-r.bitcoinchain.com/)
- Block fees collected (average) (Collected from https://api-r.bitcoinchain.com/)
- Pool fee percentage (Uses 0% fee as default)

If any of the factors fail to be collected then profitability will not be calculated.
