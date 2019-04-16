# check_cgminer

Monitoring (Nagios) plugin for checking s9 miner temperatures from the cgminer API.

## Help

```
check_cgminer - Nagios monitoring plugin for cgminer API

Usage: check_cgminer [-v|--version] [-H|--host HOST] [-P|--port PORT]
                     [-t|--temp_warn NUMBER] [-T|--temp_crit NUMBER]
                     [-r|--hash_warn NUMBER] [-R|--hash_crit NUMBER]
                     [-f|--fan_low_warn NUMBER] [-F|--fan_low_crit NUMBER]
                     [-n|--fan_high_warn NUMBER] [-N|--fan_high_crit NUMBER]
  Return Nagios formatted string based cgminer API returned values

Available options:
  -h,--help                Show this help text
  -v,--version             Show version information
  -H,--host HOST           Hostname of cgminer API (default: "127.0.0.1")
  -P,--port PORT           Port of cgminer API (default: "4028")
  -t,--temp_warn NUMBER    Warning temperature threshold in
                           Celsius (default: 90.0)
  -T,--temp_crit NUMBER    Critical temperature threshold in
                           Celsius (default: 100.0)
  -r,--hash_warn NUMBER    Warning hash rate threshold in Gh/s (default: 4000.0)
  -R,--hash_crit NUMBER    Critical hash rate threshold in
                           Gh/s (default: 3000.0)
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
```

## Usage


```
$ check_cgminer -H 10.0.0.55 -n 999
WARNING: Fan speed exceeds warning threshold of 999.0 RPM | fan6=4080.0;;;0.0;20000.0 fan5=5640.0;;;0.0;20000.0 chain_rate8=4537.97;4000.0;3000.0;0.0;10000.0 chain_rate7=4736.95;4000.0;3000.0;0.0;10000.0 chain_rate6=4556.04;4000.0;3000.0;0.0;10000.0 temp2_8=74.0;90.0;100.0;20.0;120.0 temp8=55.0;90.0;100.0;20.0;120.0 temp2_7=71.0;90.0;100.0;20.0;120.0 temp7=54.0;90.0;100.0;20.0;120.0 temp2_6=75.0;90.0;100.0;20.0;120.0 temp6=59.0;90.0;100.0;20.0;120.0
```
