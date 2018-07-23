# Sneaky Implicit Mutation

![](https://cdn-images-1.medium.com/max/800/1*7mPFT73sqID-Qu_TGhEL4Q.png)

In this short post I wanted to go through a wall I ran into (head first)
and how I eventually was told how to run around it instead. Thank you so
much \@joncfoo in the \#haskell channel of [fpchat
Slack](https://fpchat-invite.herokuapp.com/)!

My task seemed simple at first. I wanted to write to a temporary file,
create a `conduit` `Source` from the handle and check that `bracketP` did its clean up job and I read back the correct bytes.
The handle was being closed so hooray for that! But I kept seeing this:

```haskell
Falsifiable (after 1 test):

"a" /= ""
```

What the hell? Why am I getting back no data? I decided to write a stack
script to replicate this test on a smaller case:

```haskell
#!/usr/bin/env stack

import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified System.IO as IO

main :: IO ()
main = do
  let s = "a"
      bytes = BC.pack s

  handle <- IO.openFile "streaming_test.txt" IO.ReadWriteMode
  _ <- IO.hSetBuffering handle IO.NoBuffering

  -- Write "a" to file
  _ <- B.hPut handle bytes

  -- Source the handle and consume the written content
  res <- sourceHandle handle $$ CL.consume

  -- Close handle
  _ <- IO.hClose handle


  -- Check result
  print $ "Result: " ++ show res
```

Sure enough the output would be `"Result []"` so I share my woes in \#haskell. The issue was that
when I wrote to the `Handle` the
`"a"` would be in the file but the
`Handle` is using a file pointer and is
pointing at the next position. It's been a long time since I had to
think about seeking in files...

The solution in this case is to seek back to the beginning of the file
after writing to it using:

```haskell
IO.hSeek handle IO.AbsoluteSeek 0
```

### Conclusion

The lesson I learned here was when you're in `IO` always watch your back because you're working with the
RealWorld. On top of this you should probably not being doing reading
and writing of files in the same phase of your program, but
unfortunately this was for some resource testing so it had to be done.
Hopefully this will stop someone going through the same headache I went
through!

If I've missed something or said something that's not correct then hit
me up :)
