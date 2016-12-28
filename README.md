# Haskell Fun
> "Do or do not, there is no try" ~ Master Yoda

<img style="text-align:center;" src="http://cdn.usefulstuff.io/2016/03/haskell-logo.jpg">

I decided to pick up Haskell for the fun of learning a truly strict functional programming language besides F#, which we used at Florida International University's **Survey Programming Languages** class. 

Here is a sampler of how Haskell code looks like!
```haskell
main :: IO()
main = do
  putStrLn "Hello World!"
```

If we wanted to be more verbose:
```haskell
module Main where 

import Prelude (putStrLn)

main :: IO()
  Prelude.putStrLn "Hello World!"
```

## Resources

In this repository, I try to follow multiple resources, but mostly following the format of these classes:

  * [UPenn's CIS194 Introduction to Haskell](http://www.seas.upenn.edu/~cis194/spring13/)
  
    This class assumes no prior knowledge with functional programming nor with programming in general (at least, that's how I feel).
    
    
  * [Stanford's CS240H Functional Systems in Haskell](http://www.scs.stanford.edu/16wi-cs240h/)
    
    This class goes into a deeper dive of using Haskell for building functional systems, although Week 1 practically goes through a very quick overview of what is covered on the above course. Mainly helpful for getting a taste of how to do low-level stuff in Haskell.
    
    
  * [Stanford's CS229 Machine Learning](http://cs229.stanford.edu/materials.html)
    
    This class goes into depth about everything Machine Learning, but does not include anything up to and past the level and complexity of Neural Networks. I mainly consulted this class because I get bored with doing pure homework assignments from the UPenn class from time to time, so I take this class, do the problem sets and take notes and implement the Machine Learning algorithms in Haskell!
    
    
I also consult these resources quite frequently:

  * [Haskell The Hard Way](http://yannesposito.com/Scratch/en/blog/Haskell-the-Hard-Way)
  
    A nice blog post that gets through a lot of the important stuff in Haskell very quickly. Mostly consulted this for the IO and Monads Sections.
    
  * [Hoogle](http://hoogle.haskell.org)
  
    Practically a search engine for all of the available Haskell modules, functions and so forth. It's a cool resource because you can search using type signatures, which means you can search functions not only by names, but something like:
    
    ```haskell
    [Int] -> Int -> Int
    ```
    
    which searches for all functions that accept a list of Integers and an Integer for inputs, and outputs an Integer

## Why learn Haskell?

  * **Unlike F# and Scala, it doesn't make any compromise by allowing you to use OOP where convenient.** This means that from here on out, it's full steam ahead for Functional Programming - no exceptions! A brand new way of thinking and looking at how to solve problems!
  
  
  * **Wanted another functional programming paradigm perspective away from F#.** I also considered picking up Haskell since a while back because of the once-corporate license that was present to using F#. However, since that time (about 2-3 years ago), **Microsoft has proudly open-sourced .NET Core**, which includes the F# family! Check out details [here](http://fsharp.org) and [here](https://dotnetfoundation.org)
  
  
  * **Because it's hipster tech.** It's nice to get first dibs on a programming language with a smaller community
  
  
  * **Offers an interesting take on managing state (or the lack thereof).** Since there is no facility in the language that allows for mutable variables, at any point in time, you are working with immutable - unchangeable values. Haskell throws a fit when you try to change one! Working in a purely functional way offers interesting ways to make code structured, testable (test-driven development) and easily parallelizable from the get go.
  