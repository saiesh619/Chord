# Project 3 – Chord Protocol Simulation

## Team Members

* Saiesh Prabhu
* Pranav Anne

## Overview

This project implements the **Chord protocol** (as specified in Section 4 of *Chord: A Scalable Peer-to-Peer Lookup Service for Internet Applications* by Stoica et al.) using the **actor model** in Gleam.
The system supports:

* Network **join** operations.
* **Lookup/routing** across a distributed hash table (DHT).
* A simple key–value application where each key (same identifier space as Chord node IDs) is associated with a string.

Each peer in the system is represented as an **actor**. Peers can join, maintain finger tables, and route messages. The simulation measures the efficiency of lookups by tracking the number of hops required to resolve requests.

---

## Input Format

The program is run with the following command-line format:

```
gleam run
```

* `numNodes`: Number of peer actors to create in the Chord ring.
* `numRequests`: Number of lookup requests each peer makes.
* Each peer issues **1 request per second**.
* Once all requests are completed, the program terminates.
* `numNodes` and `numRequests` can be changed from the main function
---

## Output Format

When the simulation completes, the system prints:

```
All lookups finished. Average hops = X
Simulation complete.
```

Where `X` is the **average number of hops** (actor-to-actor messages) needed across all lookups.

---

## Actor Modeling

* **One actor per peer** in the Chord ring.
* Each actor maintains:

  * A unique identifier in the `m`-bit space.
  * Successor and predecessor pointers.
  * A finger table for efficient routing.
* Actors handle messages such as `Join`, `FindSuccessor`, `Notify`, `Stabilize`, and `FixFingers`, following the Chord API.
* Periodic stabilization ensures the ring remains consistent under dynamic conditions.

---

## What is Working

* **Chord ring creation** and node joins.
* **Finger table initialization and updates**.
* **Routing lookups** for arbitrary keys.
* **Hop counting** across the entire simulation.

---

## Largest Network Tested

* Successfully tested with **N = 256 nodes**, each performing multiple requests.
* Average hop count scales close to the expected **O(log N)** behavior (e.g., ~5–8 hops for 256 nodes, compared to log₂(256) = 8).
* The program works for values beyond that but takes longer time to compile
* 300, 20 also works


## Bonus: Failure Model and Resilience Testing

### Attempt

The assignment offered a 20% bonus for implementing node and connection failure handling (as described in Section 5 of the Chord paper). I did **not manage to fully implement** this part. However, I did explore what would be required and attempted small tests to understand how resilient the system could be.

---

### What I Tried

* **Node Failures:** I tried simulating a peer “dying” by stopping its lookups/messages manually. The system could not properly recover because I only had a single successor pointer.
* **Connection Failures:** I attempted to simulate message drops/delays, but without retries or timeout handling, the network stalled.
* **Multiple Failures:** When removing more than one node, the ring often broke completely, showing that the current design (no successor list, no replication) is not resilient.

---

### Findings

* The current system is **not resilient** to failures. Even one dead node can break the ring and prevent lookups from succeeding.
* Stabilization alone is not enough; it needs **successor lists** and **key replication** as described in the paper.
* I confirmed through these failed attempts that the current implementation is **correct only in the no-failure model**.

---

### Conclusion

I was not able to implement failure handling. My attempts showed that:

* Without successor lists and key replication, the system fails quickly under churn.
* To complete this bonus, I would need to extend stabilization with successor lists, retries, and replication.

---
