# check_cgminer

[![CircleCI](https://circleci.com/gh/dmp1ce/check_cgminer.svg?style=svg)](https://circleci.com/gh/dmp1ce/check_cgminer)

Monitoring (Nagios) plugin for checking cgminer miner devices statistics from the cgminer API.

Tested devices:

- DR5 (no automatic profitability detection)
- S9
- S9k (no automatic profitability detection)
- S9se
- S15
- S17
- Z9-mini (no automatic profitability detection)
- Whatsminer (no automatic profitability detection)

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
```

## Usage


```
$ check_cgminer -H 10.0.0.55 -n 999
WARNING: Fan speed exceeds warning threshold of 999.0 RPM | fan6=4080.0;;;0.0;20000.0 fan5=5640.0;;;0.0;20000.0 chain_rate8=4537.97;4000.0;3000.0;0.0;10000.0 chain_rate7=4736.95;4000.0;3000.0;0.0;10000.0 chain_rate6=4556.04;4000.0;3000.0;0.0;10000.0 temp2_8=74.0;90.0;100.0;20.0;120.0 temp8=55.0;90.0;100.0;20.0;120.0 temp2_7=71.0;90.0;100.0;20.0;120.0 temp7=54.0;90.0;100.0;20.0;120.0 temp2_6=75.0;90.0;100.0;20.0;120.0 temp6=59.0;90.0;100.0;20.0;120.0
```

## Profitability

`check_cgminer` will try to automatically detect the miners profitability by getting several factors.

- Power consumption (Attempts to auto detect miner and use hard coded wattage values by default)
- Electricity rate (Uses USA national average by default)
- Bitcoin price (Collected from https://apiv2.bitcoinaverage.com/
- Difficulty (Collected from https://api-r.bitcoinchain.com/)
- Block reward (Currently hardcoded at 12.5 BTC)
- Block fees collected (average) (Currently hardcoded at 0 BTC)

If any of these factors fail to be collected then profitability will not be calculated.
