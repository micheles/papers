Tips for the Scientific Programmer
----------------------------------

Michele Simionato@[GEM Foundation](https://www.globalquakemodel.org)

+++?image=risk_map_small.png

+++

**This talk is not about micro-optimizations**

- old tricks like replacing
  sqrt(x^2 + y^2) < R => x\*x + y\*y < R\*R
  are not really interesting
- what makes the difference is using the right library
  (i.e. *scipy.spatial.distance*)
- *and* using the right architecture

---

**Input/output formats**

- *never*, EVER change the input formats
- there is more freedom with the output formats

---

**Inputs formats we are using**

- INI (good, but TOML would have been better)
- XML/NRML/XSD (could have been simpler)
- CSV (should have been used more)
- HDF5 (UCERF3, GMPE tables)

+++

**Output formats we are using**

- XML / NRML: we are removing it
- CSV with pre-header: we are using it more and more
- HDF5: used sometimes
- NPZ: by necessity

+++?image=hazard_outputs.png

+++

**Internal formats we are using**

- .hdf5
- .toml

---

**The choice of the input/outputs format has a big performance impact**

- XML/CSV exporters
- XML/CSV importers
- Clearly the choice of the internal formats is even more important:
  HDF5 is the way to go

---

**what's behind: @color[gray](celery/rabbitmq)**

![rabbitmq](rabbitmq.png)

- celery/rabbitmq is not ideal for our use case
- we have celery for legacy reasons and because it sort of worked until now
  
+++

**what's behind: @color[gray](celery/rabbitmq)**

- rabbitmq is meant for lots of small messages, but instead we have few
  huge messages
- rabbitmq is meant for resilience more than performance
- it stores everything in the mnesia
  database, but we do not need that and it is counterproductive
- we had problems with specific versions of rabbitmq
- lots of configurations the users can get wrong

+++

**what's behind: @color[gray](celery/rabbitmq)**

![celery](celery.jpeg)

- using celery/redis did not work out (missing revoke)
- the default configuration is not the ideal one for us
  (*worker_prefetch_multiplier*, *result_cache_max*)
- celery was keeping in memory all task results and we had to monkey patch it
- we had celery waiting for already completed tasks :-(

+++

**what's behind: @color[gray](celery/rabbitmq)**

- celery has 50,000+ lines of code, while rabbitmq is in Erlang
- if something does not work, you are dead (but the mailing list is helpful)
- there are strong limits on the amount of data you can transfer in a
  short time

---

**what's behind: @color[green](zmq)**

![zeromq](zeromq-logo.jpg)

- I studied the zmq book after EuroPython 2017
- I implemented what I needed in 2-3 days @fa[thumbs-up]
- it worked really well, even if not at the first attempt

+++

**what's behind: @color[green](zmq)**

- data is sent back via zmq now (PUSH/PULL pattern)
- we bypassed celery/rabbitmq limits completely
- one must be careful: packets keep circulating
- we have a plan B if celery/rabbitmq should fail us again, but we are
  not going to reinvent the wheel

---

**what we might be using: @color[gray](dask)**

- not used until now because we are conservative
- dask documentation has improved a lot
- `dask.distributed.Client.map` is the easy migration path I was looking for
- we are not using it in production but we have experimental support for it
- we'd love to hear from you :-)

---

**what we are NOT using**

- @color[red](C extensions)
- @color[red](Cython)
- @color[red](numba)
- @color[red](Intel Python)

+++

**@color[red](C extensions)**

- we had C extensions in the past, before the wheels
- gcc was required to install the software
- otherwise a slower fallback was used
- there was duplication between Python and C
- migrating to Python 3 would have been hard
- but I was able to speedup the Python(numpy) part!
- now the code base is 100% pure Python @fa[thumbs-up]

+++

**@color[red](Cython)**

- Cython is better than plain C
- and we have wheels now
- however I could not get impressive speedups
- in any case most our code is not suitable for Cythonization
- it was deemed not worthy

+++

**@color[red](numba)**

- I tried it on our code computing the distances
- the only way I could get a substantial speedup was by using the parallel
  option
- but we are already parallelizing, so we would have risked oversubscription
- I was also worried about memory consumption
- at the end @color[green](scipy.spatial.distance) saved the day

+++

**@color[red](Intel Python)**

- there are some concerns about vendor locking, of course
- but the real reason is that I got a 20% slowdown, even yesterday
- we will wait another year

---

@color[green](@size[2em](Algorithmic solutions are always better than technological solutions))

---?image=collapse.jpg

@color[white](what about architecture?)

+++

- however good the underlying libraries, your software will collapse
  if you have the wrong architecture (speaking from experience)

+++

**some bottleneck are less obvious**

- if transferring data is the big issue, consider using a distributed filesystem
- NFS was really good for us

+++

**some bottleneck are just not there**

- if you need to write a lot of data the single writer
  architecture scales a lot more than one could expect (> 10 GB/minute)
- tip: disabling the swap is a good idea

+++

- for speed, it is *essential* to find out the right data structure in HDF5
- at the end a structured array plus a dataset with variable-length indices
  was the best approach for the GMFs

---

**more on h5py/hdf5**

- h5py is really nice and Pythonic (but comes with caveats)
- it makes a lot of sense to serialize Python objects to HDF5
- it was easy to define a serialization protocol:

```python
    def __toh5__(self):
        return self.grouparray, self.attrs
        
    def __fromh5__(self, grouparray, attrs):
        self.grouparray = grouparray
        self.attrs = attrs
```
+++

**there is an OpenQuake HDF5 serialization library**

part of https://github.com/gem/oq-engine

*(free, with AGPL licence)*

```python
    with openquake.baselib.hdf5.File('x.hdf5', 'w') as f:
         f[key] = obj 
        
    with openquake.baselib.hdf5.File('x.hdf5', 'r') as f:
         obj = f[key] 
```

- we are converting our input files from XML to HDF5
- USGS will provide ShakeMaps in HDF5 format
- if you are not using HDF5, you should ;-)
