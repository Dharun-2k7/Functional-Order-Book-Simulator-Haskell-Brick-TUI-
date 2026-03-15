# Functional Order Book Simulator (Haskell)

## Overview

This project is a simple **Order Book Simulator** implemented in **Haskell**.  
An order book is a core component of financial exchanges where buy and sell orders are stored and matched. It is used in stock markets, cryptocurrency exchanges, and trading platforms.

The purpose of this project is to demonstrate how the basic logic of a trading engine can be implemented using **functional programming concepts** in Haskell.

---

## Why an Order Book?

Order books are fundamental to modern trading systems. They maintain two lists:

- **Buy Orders (Bids)** – traders willing to buy at a certain price  
- **Sell Orders (Asks)** – traders willing to sell at a certain price  

When a buy price is greater than or equal to a sell price, a **trade is executed**.

Building an order book simulator helps in understanding:

- How exchanges process orders
- Price priority in trading systems
- Basic matching engine logic
- Data structures used in financial software

---

## Why Haskell?

Most real-world trading engines are written in **C++** because of its extremely low latency and high performance. However, Haskell is still a reasonable choice for implementing an order book simulator because it emphasizes **clear and reliable program design**.

Using Haskell allows us to focus on:

- **Pure functions**
- **Immutable data structures**
- **Pattern matching**
- **Recursive algorithms**
- **Clear logic without side effects**

Although Haskell may not match the raw execution speed of C++, it provides a **clean and expressive way to model complex systems** like trading engines.

---

## Features

- Representation of buy and sell orders
- Order insertion based on price priority
- Automatic order matching
- Trade quantity updates
- Simulation of sequential order processing
- Implementation using functional programming techniques

---

## Concepts Used

This project demonstrates several core Haskell concepts:

- Algebraic Data Types
- Pattern Matching
- Recursion
- Immutable Data Structures
- List Manipulation
- Pure Functions

---

## Example

Example orders processed by the simulator:

Buy Order: Price = 100, Quantity = 10  
Sell Order: Price = 99, Quantity = 5  

Since the buy price is greater than the sell price, a trade occurs and the quantities are updated accordingly.

---
## Purpose of This Project

This project was developed as part of a **Haskell mini project** to demonstrate how functional programming concepts can be applied to real-world system designs.

The goal was to implement a simplified **order book and matching engine**, which is a core component of financial trading platforms.

Through this project, concepts such as **algebraic data types, pattern matching, recursion, immutable data structures, and list manipulation** are applied to simulate how buy and sell orders are stored and matched.

While real-world trading engines are often implemented in high-performance languages like **C++**, this project shows how the same core logic can be modeled clearly using **Haskell’s functional programming approach**.
