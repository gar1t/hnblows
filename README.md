Work in progress for Psychobitch demo.

## Scaling

- local port range

Increase to full range:

    echo "1024 65535" > /proc/sys/net/ipv4/ip_local_port_range

- ulimit -n

Increase using:

    ulimit -n 999999

- Erlang process limit

Use +P to increase as needed.

## References

http://www.metabrew.com/article/a-million-user-comet-application-with-mochiweb-part-1
