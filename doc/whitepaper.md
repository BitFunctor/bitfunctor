BitFunctor Project
==================

A decentralized system for certified code storage and reuse


> Whitepaper Draft, 29 June 2016
> 
> Andrey Lyashin <al@bitfunctor.net>, Sergey Kazenyuk <sk@bitfunctor.net>


Abstract
--------

BitFunctor is a blockchain-based decentralized storage of a certified software code with a number of innovative blockchain platform features like advanced monetary system with options, generalized states and generalized assets, together with services-on-the-blockchain, facilitating its use as an ultimate tool for certified code developers in Coq, Agda, and Idris programming languages.

Being in essence “a decentralized github” for mathematically-correct code, BitFunctor encourages more contributions by the community members as well as their reusability through author incentivization with the unique option-based approach.

Contribution incentivization model is based on options' emission as follows: exactly one option (cBTF) is being issued for each and every user's contribution, which can later be exchanged to built-in asset (BTF) at the valuation at the time of the exchange.

Network-support is incentivized by block rewards, the latter depending on the cumulative complexity of block verification, encouraging more "valuable" transactions to be confirmed earlier.

Just as with any other asset types, both BTFs and cBTFs can be freely transferred. BTFs, though, can be emitted only by the means block generation, while cBTF – by the means of contribution to the decentralized code storage.

BitFunctor's objective is to bring advanced mathematical concepts and tools used in theorem proving and certified programming to the industry, improving quality and reliability of the enterprise applications, mission critical services, and financial smart-contracts.


Main features
-------------

* Blockgraph-based PoS consensus algorithm
* Classical (BTF) and option (cBTF) assets
* Generalized assets
* Generalized specialized states
* Services-on-the-blockchain


Generalized assets
------------------

In addition to two built-in asset types, BTF and cBTF, BitFunctor allows one to emit his own assets, similar to functionality present in NXT and Waves.


Generalized specialized states
------------------------------

BitFunctor comes with two built-in states:

* ledger – account balance storage for all built-in and user-emitted assets
* code storage state – certified code storage


Nevertheless the systems works with the global states in a general way – nothing is hardwired and everything is easy to change.


This allows to support a specialized application-specific state storage on the single blockchain. Every transaction may potentially contain unlimited number of state modificators, each for their own specialized state.

A node is not required to support all specialized states existing on the blockchain, only those that matter for the node are going to change the specific state, the unsupported state modificators (specialized states) are ignored.

State modificator – the formal specification of the change the transaction makes on the state storage, e.g. asset transfer on the ledger state (move 3.5BTF from account A to B).

Generalized specialized states allow one to build an unlimited number of well-known as well as novel applications like decentralized games on the blockchain, smart-contracts. etc.


Services-on-the-blockchain
--------------------------

Service-on-the-blockchain provides off-chain services for other actors on the blockchain (users or other services), consuming and producing the data according to its formal specification published on the blockchain service registry (blockchain service marketplace).


Target Audience
---------------

Certified programming developers or theorem-proving software users:

* Enterprise users
* Scientists and researchers
* Geeks.
