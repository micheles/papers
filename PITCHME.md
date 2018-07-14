Python in scientific computing
------------------------------

*what works and what doesn't*

Michele Simionato [GEM Foundation](https://www.globalquakemodel.org)

+++

![california](hazard_map.png)

---

**What I mean by scientific computing**

- Distributed numerical simulations
- Lots of data being generated
- Nontrivial postprocessing
- Issues with CPU time, memory and data transfer

+++

![all-cores](all-cores.png)

---

**numpy**

- numpy is good
- had troubles with returning back structured arrays in earlier versions

---

**scipy**

- scipy is good
- and it keeps improving

---

**h5py/hdf5**

- it is good but tricky
- lots and lots of issues with structured arrays/variable length arrays
- several issues with bytes/strings at the time of the Python 2->3 migration
- recent debacle with the HDF5 1.8 -> 1.10 upgrade
- silx view is a lot better than HDFView!
