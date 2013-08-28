## erlsh

Family of functions involving interacting with the system shell and paths.

Reason why not `os:cmd/1`:

```erlang
> Email = "hacker+/somepath&reboot@example.com". % this is a valid email!
> os:cmd(["mkdir -p ", Email]).
% path clobbering and a reboot may happen here!
```

Examples with `erlsh:run/1,2,3,4`, `erlsh:oneliner/1,2`, `erlsh_path:escape/1`:

```erlang
> erlsh:oneliner("uname -v"). % oneliner/1,2 funs do not include newlines
{done,0,
      <<"Darwin Kernel Version 12.4.0: Wed May  1 17:57:12 PDT 2013; root:xnu-2050.24.15~1/RELEASE_X86_64">>}

> erlsh:oneliner("git describe --always").
{done,128,<<"fatal: Not a valid object name HEAD">>}

> erlsh:oneliner("git describe --always", "/tank/proger/vxz/otp").
{done,0,<<"OTP_R16B01">>}

> erlsh:run(["git", "clone", "https://github.com/proger/darwinkit.git"], binary, "/tmp").
{done,0,<<"Cloning into 'darwinkit'...\n">>}

> UserUrl = "https://github.com/proger/darwinkit.git".
"https://github.com/proger/darwinkit.git"
> erlsh:run(["git", "clone", UserUrl], binary, "/tmp").
{done,128,
      <<"fatal: destination path 'darwinkit' already exists and is not an empty directory.\n">>}

> Path = erlsh_path:escape("email+=/subdir@example.com").
"email+=%47subdir@example.com"

> erlsh:oneliner(["touch", filename:join("/tmp/", Path)]).
{done,0,<<>>}

> erlsh:run(["ifconfig"], "/tmp/output.log", "/tank/proger/vxz/otp").
{done,0,"/tmp/output.log"}

% cat /tmp/output.log
>>> {{2013,8,28},{8,39,14}} /sbin/ifconfig
lo0: flags=8049<UP,LOOPBACK,RUNNING,MULTICAST> mtu 16384
	options=3<RXCSUM,TXCSUM>
	inet6 fe80::1%lo0 prefixlen 64 scopeid 0x1
	inet 127.0.0.1 netmask 0xff000000
	inet6 ::1 prefixlen 128
gif0: flags=8010<POINTOPOINT,MULTICAST> mtu 1280
stf0: flags=0<> mtu 1280
en0: flags=8863<UP,BROADCAST,SMART,RUNNING,SIMPLEX,MULTICAST> mtu 1500
	ether 7c:d1:c3:e9:24:65
	inet6 fe80::7ed1:c3ff:fee9:2465%en0 prefixlen 64 scopeid 0x4
	inet 192.168.63.163 netmask 0xfffffc00 broadcast 192.168.63.255
	media: autoselect
	status: active
p2p0: flags=8843<UP,BROADCAST,RUNNING,SIMPLEX,MULTICAST> mtu 2304
	ether 0e:d1:c3:e9:24:65
	media: autoselect
	status: inactive
>>> {{2013,8,28},{8,39,14}} exit status: 0
```
