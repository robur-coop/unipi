# Unipi - serve git repository content as HTTPS

Unipi is a MirageOS unikernel that provides the contents of a git repository via
HTTP and HTTPS. It embeds (optional) let's encrypt provisioning.

A webhook is provided to update the internal state of the git repository. An
example deployment is [test.nqsb.io](https://test.nqsb.io), which serves the
[ocaml-dns](https://github.com/mirage/ocaml-dns) documentation (using the
gh-pages branch).

Inspiration for this unikernel is taken from
[Canopy](https://github.com/Engil/Canopy) after discussion with the
[Muen](https://muen.sk) developers.

## Configuration

Unipi is only configured via boot parameters, as follows:

- `--port` configures the TCP port to listen on (defaults to 80)
- `--remote` configures the git remote url (e.g. https://github.com/mirage/ocaml-dns.git#gh-pages)
- `--hook` configures the (secret) url of the webhook, if requested unipi updates its internal git remote
- `--ipv4` configures the IPv4 address of unipi (e.g. 192.168.2.2/24)
- `--ipv4-gateway` configures the IPv4 gateway

To use git via ssh (only public/private key authentication is supported):
- `--ssh-seed` for the seed of the private key (use `awa_gen_key` to produce a random seed and public key)
- `--ssh-authenticator` to authenticate the ssh remote (format is SHA256:b64-encoded-public-key hash, the output of `ssh-keygen -lf <(ssh-keyscan -t rsa remote-host 2>/dev/null)`)

For HTTPS service with let's encrypt certificate:
- `--tls=true` enables tls
- `--hostname=my-server.example.com` configuring the server name
- `--production=true` for let's encrypt production environment (default is staging)
- `--cert-seed=<my-seed>` seed for the private key of the certificate (`dd if=/dev/random bs=32 count=1 | b64encode -m -`)
- `--account-seed=<my-seed>` seed for the let's encrypt account (see above)
- `--email=<account email>` let's encrypt account email address

## Installation from source

To install unipi from source, you need to have [opam](https://opam.ocaml.org)
(>= 2.0.0) and [ocaml](https://ocaml.org) (>= 4.07.0) installed. Also,
[mirage](https://mirageos.org) is required (>= 3.7.6) (follow the
[installation instructions](https://mirageos.org/wiki/install)).

```bash
$ git clone https://github.com/hannesm/unipi.git
$ mirage configure -t <your-favourite-target>
$ make depend
$ make
```
