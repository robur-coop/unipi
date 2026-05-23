# Unipi - serve git repository content as HTTPS

Unipi is a MirageOS unikernel that provides the contents of a git repository via
HTTP and HTTPS. It embeds (optional) let's encrypt provisioning.

A webhook is provided to update the internal state of the git repository. An
example deployment is [robur.coop](https://robur.coop), which serves the
[robur website](https://git.robur.coop/robur/homepage-data) (using the
pages branch).

Some HTTP headers are added: "content-type", which value is looked up of the
file ending using [magic-mime](https://github.com/mirage/ocaml-magic-mime/).
"last-modified" and "etag" are the timestamp of the most recent commit to the
git remore, respective the hash of the most recent commit. If the client sends
"if-modified-since" or "if-none-match", and either matches the most recent
commit, the HTTP status Not modified (304) is returned with an empty body.

Inspiration for this unikernel is taken from
[Canopy](https://github.com/Engil/Canopy) after discussion with the
[Muen](https://muen.sk) developers.

## Configuration

Unipi is only configured via boot parameters, as follows:

- `--port` configures the TCP port to listen on (defaults to 80)
- `--remote` configures the git remote url (e.g. https://github.com/mirage/ocaml-dns.git#gh-pages)
- `--hook` configures the (secret) url of the webhook, if requested unipi updates its internal git remote (default is "hook")
- `--ipv4` configures the IPv4 address of unipi (e.g. 192.168.2.2/24)
- `--ipv4-gateway` configures the IPv4 gateway

To use git via ssh (only public/private key authentication is supported):
- `--ssh-key` for the ssh private key - either rsa:<seed> or ed25519:<b64-encoded private key> (you can use the tool `awa_gen_key` to produce a random seed and public key)
- `--ssh-authenticator` to authenticate the ssh remote (format is SHA256:b64-encoded-public-key hash, the output of `ssh-keygen -lf <(ssh-keyscan -t rsa|ed25519 remote-host 2>/dev/null)`)

For HTTPS service with let's encrypt certificate:
- `--tls=true` enables tls
- `--hostname=my-server.example.com` configuring the server name
- `--production=true` for let's encrypt production environment (default is false, which uses the let's encrypt staging environment)
- (optional) `--cert-seed=<my-seed>` seed for the private key of the certificate (`dd if=/dev/random bs=32 count=1 | b64encode -m -`)
- (optional) `--account-seed=<my-seed>` seed for the let's encrypt account (see above how to generate this)
- (optional) `--email=<account email>` email address used for let's encrypt account registration

For a complete list of boot parameters, execute the binary with `--help` as
argument.

## Installation from source

To install this unikernel from source, you need to have
[opam](https://opam.ocaml.org) (>= 2.1.0) and
[ocaml](https://ocaml.org) (>= 4.13.0) installed. Also,
[mirage](https://mirageos.org) is required (>= 4.10.0). Please follow the
[installation instructions](https://mirageos.org/wiki/install).

The following steps will clone this git repository and compile the unikernel:

```bash
$ git clone https://github.com/robur-coop/unipi.git
$ cd unipi
$ mirage configure -t <your-favourite-target> #i.e. hvt, spt, xen
$ make depend
$ make build
```

## Installing as binary

Binaries are available at [Reproducible OPAM
builds](https://builds.robur.coop/job/unipi/), see [Deploying binary MirageOS
unikernels](https://hannes.robur.coop/Posts/Deploy) and [Reproducible MirageOS
unikernel builds](https://hannes.robur.coop/Posts/ReproducibleOPAM) for details.

## Questions?

Please open an issue if you have questions, feature requests, or comments.
