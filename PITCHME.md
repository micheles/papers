Tips for the Scientific Programmer
----------------------------------

Michele Simionato@[GEM Foundation](https://www.globalquakemodel.org)

+++?image=risk_map_small.png

+++

**This talk is not about micro-optimizations**

- we do not have full control on the source code, which is very often
  user-contributed
- profiling is essential to find bottlenecks like duplicated operations
  in inner loops, @color[grey](but I do that 1-2 times per year)
- what really makes the difference is using the @color[green](right library)
  and the right @color[green](architecture / data structures)

---

**Input/output formats**

Here I learned the hard way a very essential lesson:

@color[red](*never, EVER change the input formats*)

You cannot. Really, you can not.

There is more freedom with the output formats

+++

**Inputs formats we are using**

- INI (good, but TOML would have been better)
- XML/NRML/XSD (could have been simpler)
- CSV (should have been used more)
- HDF5 (in rare cases: UCERF3, GMPE tables)

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
- .sqlite

+++

**The choice of the input/outputs format has a big performance impact**

- XML/CSV exporters
- XML/CSV importers
- Clearly the choice of the internal formats is even more important:
  @color[green](HDF5 is the way to go)

---

**Task distribution**

- we are using *multiprocessing* on a single machine
- and celery/rabbitmq on a cluster

![rabbitmq](rabbitmq.png)
![celery](celery.jpeg)

- celery/rabbitmq is not ideal for our use case but it worked
  until now, including the @color[green](REVOKE) functionality
  
+++?image=slow-task.png
**slow tasks :-(**

+++

Slow tasks have been a PITA for years @fa[frown]

A few months ago we had a breakthrough: @color[green](subtasks)

+++

We made the output receiver able to recognize tuples of the form
`(callable, arg1, arg2, ...)`;
this made it possible to define task splitters

```python
def task_splitter(sources, arg1, arg2, ...):
    blocks = split_in_blocks(sources, maxweight)
    for block in blocks[:-1]:
         yield (task_func, block, arg1, arg2, ...)
    yield task_func(block[-1], arg1, arg2, ...)
```

+++

We introduced a task splitter able to perform a subset of the
calculation and to estimate the expected total time:
it can split the calculation in subtasks with estimated runtime smaller
that an user-given `task_duration`

+++

Successively, we made the engine smart enough to determine a sensible default
for the `task_duration`, depending on the number of ruptures, sites and levels

@color[green](=> slow tasks are greatly reduced)

@color[red](except for non-splittable sources)

---

**Data transfer**

- don't split too much, to avoid too many outputs
- we switched to using zmq to return the outputs @fa[thumbs-up]
![zeromq](zeromq-logo.jpg)
- we switched to NFS to read the inputs

---

**Memory occupation**

Another big problem we had to fight constantly is the memory occupation

Notice that running out of memory *early* @color[green](can be a good thing)
