Lessons about technologies
--------------------------

- concurrent.futures is fine
- Travis is really good
- wheels are great
- h5py is ultra-fast but can bite you

---

How we store data in the engine
-----------------------------------

- all the scientific data are in the datastore (one .hdf5 file per calculation)
- all the metadata (i.e. start/stop time of the calculation, description,
  logs, performance information, output information) are in SQLite

---

More advice
--------------------

- follow a principle of simplicity/cleanness: 95% of our speedups and
  memory saving came for free after *removing* code
- invest your time in solving the real problem, not in complicating your
  technological stack (so I did not spend time on C extensions, numbas, GPUs,
  Intel compiler, etc)
- always challenge the underlying assumptions
- take the most difficult problem *that you can solve* and solve it
  first
- most of all, be patient
