{-
 - An interpreter in Haskell for the language V by Francisco Mota.
 - See <http://esoteric.voxelperfect.net/wiki/V>.
 -
 - Written by Ørjan Johansen (2007-2017).
 -
 - This interpreter is released under the terms of the Creative Commons CC0 1.0
 - Universal Public Domain Dedication.
 -
 - May 26: Small parsing change (avoid recursion on unknown character)
 - March 26, 2014: Fix some bitrot so it compiles with modern GHC, and add
 - proper PD license.
 - July 26, 2017: Fix more bitrot by adding top level type signatures.
 -}

{-# LANGUAGE NamedFieldPuns #-}

module Main where

import System.IO
import System.Environment(getArgs)
import Control.Monad
import Control.Monad.State
import Text.Parsec

-- This should be possible to change to any Integral type, such as
-- Data.Word.Word8.
type Value = Integer

data DownTree = DownTree {
    inval :: Value, inner :: DownTree, outer :: DownTree }
data UpTree = UpTree {
    isInner :: Bool, sibval :: Value, parent :: UpTree, sibling :: DownTree }
data Root = Root {
    upIsRight :: Bool, val :: Value, up :: UpTree, down :: DownTree }
data VState = VState { root :: Root, inputHandle :: Handle }
type VMonad = StateT VState IO

main :: IO ()
main = do
    args <- getArgs
    (progHandle, progName) <- case args of
        [] -> return (stdin, "stdin")
        [f] -> liftM2 (,) (openFile f ReadMode) (return f)
        _ -> error "Only one filename argument accepted."
    uncurry (runProg progName) =<< getProg progHandle

runProg :: SourceName -> String -> Handle -> IO ()
runProg progName prog input =
    case parse program progName prog of
        Left e -> hPrint stderr e
        Right cmds ->
            runStateT cmds VState{root=emptyTree, inputHandle=input}
                >> return ()

-- One of the few things that annoy me about Haskell is how much more awkward
-- it is to parse things if you want to keep interactive control of the input,
-- since its quick and easy parsing methods are based on pure lists.
getProg :: Handle -> IO (String, Handle)
getProg h = gp "" where
    gp s = do
        e <- hIsEOF h
        if e then return (reverse s, stdin) else do
            c <- hGetChar h
            case c of
                '!' -> return (reverse s, h)
                _ -> gp (c:s)

program :: Parsec String () (VMonad ())
program = between (return ()) eof commands
commands = liftM sequence_ $ many $
    choice (zipWith (\c cmd -> char c >> return cmd) "\\/>.,"
        [descend, ascend, transfer, output, input])
    <|> liftM loop (between (char '[') (char ']') commands)
    <|> (noneOf "]" >> return (return ()))

descend, ascend, transfer, output, input :: VMonad ()

descend = modifyRoot $ \r@Root{down} ->
    if upIsRight r then -- we go down the inner branch
        Root{upIsRight=False, val=inval down, down=inner down,
            up = UpTree{
                isInner=True, sibval=val r - inval down,
                parent=up r, sibling=outer down}}
      else -- we go down the outer branch
        Root{upIsRight=False, val=val r - inval down, down=outer down,
            up = UpTree{
                isInner=False, sibval=inval down,
                parent=up r, sibling=inner down}}

ascend = modifyRoot $ \r@Root{up} ->
    if isInner up then -- we come from the inner branch
        Root{upIsRight=False, val=val r + sibval up, up=parent up,
            down = DownTree{inval=val r, inner=down r, outer=sibling up}}
      else -- we come from the outer branch
        Root{upIsRight=True, val=val r + sibval up, up=parent up,
            down = DownTree{inval=sibval up, inner=sibling up, outer=down r}}

transfer = modifyRoot tr where
    tr r@Root{down=down@DownTree{inner,outer}} =
        r{down=DownTree{
            inval=inval down + adjInner,
            inner=inner{inval=inval inner + adjInner},
            outer=outer{inval=inval outer - adjInner}}}
      where
        -- adjustment to the inner branch of the root
        adjInner = if upIsRight r then 1 else -1

loop :: VMonad a -> VMonad ()
loop cmds = do
    v <- gets (val . root)
    when (v/=0) (cmds >> loop cmds)

-- Sometimes pointfree style is irresistible
output = io . putChar . toEnum . fromIntegral . val . root =<< get

input = modifyRoot . setr =<< io . gc =<< gets inputHandle
  where
    gc h = do
        e <- hIsEOF h
        if e then return 0 else liftM (fromIntegral . fromEnum) $ hGetChar h
    setr v r@Root{down= ~down} =
        if upIsRight r then r{val=v} -- left branch is outer
          else -- left branch is inner
            r{val=v, down=down{inval=inval down + (v - val r)}}

modifyRoot :: (Root -> Root) -> VMonad ()
modifyRoot f = modify $ \s@VState{root=r} -> s{root=f r}

-- A tree of all zeros with the root at its outer rightward slope
emptyTree :: Root
emptyTree = Root { upIsRight = False, val = 0, up = eu, down = ed } where
    eu = UpTree { isInner = False, sibval = 0, parent = eu, sibling = ed }
    ed = DownTree { inval = 0, inner = ed, outer = ed }

io :: IO a -> VMonad a
io = liftIO

-- F. Mota's translation scheme from Brainfuck.
bftrans :: String -> String
bftrans = concatMap (maybe "" id . flip lookup bftab)
bftab :: [(Char, String)]
bftab = [
    ('>', "\\"),
    ('<', "/\\/"),
    ('+', ">"),
    ('-', "\\/>\\/"),
    ('[', "\\[/\\/"),
    (']', "\\]/\\/"),
    ('.', "\\./\\/"),
    (',', "\\,/\\/")]
