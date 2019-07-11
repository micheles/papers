Tips for the Scientific Programmer
----------------------------------

Michele Simionato@[GEM Foundation](https://www.globalquakemodel.org)

+++?image=risk_map_small.png

+++

**This talk is about "Middle Performance Computing"**

- profiling is invaluable for finding bottlenecks like slow operations
  in inner loops, @color[grey](but I do that 1-2 times per year)
- what it is really essential is @color[green](instrumenting) your code
- what makes the difference is using the @color[green](right library)
  and the right @color[green](architecture / data structure)

---

**Input/output formats**

- I learned the hard way a very essential lesson:
  @color[red](*never, EVER change the input formats*)
- You cannot. Really, you can not.
- Even if it is impossible to get right
  the input format at the beginning @fa[frown]
- There is more freedom with the output formats
- Where you can really work is on the @color[green](internal formats)

+++

**Inputs formats we are using**

- INI (good, but @color[green](TOML) would have been better)
- @color[red](XML/NRML/XSD) (could have been simpler)
- @color[green](CSV) (should have been used more)
- HDF5 (in rare cases: UCERF3, GMPE tables)
- @color[green](ZIP) (okay)

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

@color[green](They are good) @fa[thumbs-up]

+++

**The choice of the data format has a big performance impact**

- XML/CSV exporters
- XML/CSV importers
- clearly the choice of the internal formats is even more important:
  @color[green](HDF5 is the way to go)

---

**Task distribution**

- we are using *multiprocessing/zmq* on a single machine
- and *celery/rabbitmq/zmq* on a cluster

![rabbitmq](rabbitmq.png)
![celery](celery.jpeg)
![zmq](zeromq-logo.jpg)

- celery/rabbitmq is not ideal for our use case but it works enough,
  including the @color[green](REVOKE) functionality
  
+++?image=slow-task.png
**our biggest issue :-(**

+++

**Slow tasks**

- slow tasks have been a PITA for years @fa[frown]
- a few months ago we had a breakthrough: @color[green](subtasks)
- we made the output receiver able to recognize tuples of the form
  `(callable, arg1, arg2, ...)` and to send them as subtasks

+++

- task producing subtasks:
```python
def task_splitter(sources, arg1, arg2, ...):
    blocks = split_in_blocks(sources, maxweight)
    for block in blocks[:-1]:
         yield (task_func, block, arg1, arg2, ...)
    yield task_func(block[-1], arg1, arg2, ...)
```
- heavy tasks can be split in many light tasks
- the weight of the seismic sources is the number of earthquakes it can produce
- it can be *very* different from the duration of the calculation

+++

**Calibrating the computation**

- we introduced a task splitter able to perform a subset of the
  calculation and to @color[green](estimate) the expected task duration
  depending on the weight
- it can split the calculation in subtasks with estimated runtime smaller
  that an user-given `task_duration` parameter

+++

**Automatic task splitting**

- successively, we made the engine smart enough to determine a sensible default
  for the `task_duration`, depending on the number of ruptures, sites and levels
- @color[green](=> slow tasks are greatly reduced)
- @color[red](except for non-splittable sources)

---

**Solving the data transfer issue**

- we switched to using zmq to return the outputs @fa[thumbs-up]
- we switched to NFS to read the inputs (and it is also useful for
  @color[green](sharing) the code)
- IMPORTANT: do not produce too many tasks, the data transfer will kill
  you, or the output queue will run out of memory, or both

---

**Memory occupation**

- a big problem we had to fight constantly is running out of memory
  (even with 1280 GB split on 10 machines)
- notice that running out of memory *early* @color[green](can be a good thing)
- it is all about the tradeoff memory/speed
- memory allocation can be the dominating factor for performance

+++

**How to reduce the required memory**

- use as much as possible numpy arrays instead of Python objects
- use a point-by-point algorithm if you really must
- remember that big tasks are still better, if you have enough memory
- we measure the memory with `psutil.Process(pid).memory_info()`

+++

**Yielding partial results**

```python
def big_task(sources, arg1, arg2, ...):
   accum = []
   for src in sources:
       accum.append(process(src, arg1, arg2, ...)
       if len(accum) > max_size:
          yield accum
          accum.clear()  # save memory
   yield accum
```       
Lesson: a nice parallelization framework really helps

---?image=collapse.jpg

@color[white](**Questions?**)
