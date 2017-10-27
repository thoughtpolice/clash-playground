#!/usr/bin/env runghc

{-# OPTIONS_GHC -Wall #-}
module Main
  ( main -- :: IO ()
  ) where

import           Data.Monoid
import qualified Data.Text.Lazy as T
import           Text.Read           (readMaybe)

import           Development.Shake
import           Development.Shake.FilePath

import           Clash.Driver.Types (Manifest(..))

--------------------------------------------------------------------------------

bdir :: FilePath
bdir = "build"

bld :: FilePath -> FilePath
bld f = bdir </> f

top :: FilePath
top = "Multiboot.hs"

--------------------------------------------------------------------------------

icemulti :: FilePath -> [FilePath] -> Action ()
icemulti out apps = do
  need apps
  cmd_ [EchoStderr False] "icemulti -v -A16 -p0" ["-o", out] apps

arachne :: FilePath -> FilePath -> Action ()
arachne inp out = do
  need [ inp ]
  let log' = FileStderr (out -<.> "route.rpt")
  cmd_ log' "arachne-pnr -d 1k -P tq144 -p icestick.pcf" ["-o", out] inp

icepack :: FilePath -> FilePath -> Action ()
icepack inp out = do
  need [ inp ]
  cmd_ "icepack" inp out

icetime :: FilePath -> FilePath -> Action ()
icetime inp out = do
  need [ inp ]
  cmd_ [FileStdout out] "icetime -tmd hx1k -c 25" inp

yosys :: FilePath -> Action ()
yosys script = do
  need [ script ]
  let out = script -<.> "synth.rpt"
  cmd_ [FileStdout out] "yosys" ["-s", script]

clash :: FilePath -> Action ()
clash inp = do
  let objdir = bld "objs"
  let args = [ "-v0", "--verilog"
             , "-fclash-hdldir", bdir
             , "-hidir", objdir, "-odir", objdir
             , inp
             ]
  need [ inp ]
  cmd_ [ EchoStdout False, EchoStderr False ] "clash" args

--------------------------------------------------------------------------------

myShakeOptions :: ShakeOptions
myShakeOptions = shakeOptions
  { shakeColor = True
  }

main :: IO ()
main = shakeArgsWith myShakeOptions [] $ \_ targets -> pure $ Just $ do
  let apps = map (\x -> bld $ "app" <> show x <.> "bin") [ 0 .. 3::Integer ]

      mainhi = bld ("objs" </> top -<.> "hi")
      rtldir = bld ("verilog" </> takeBaseName top)

  -- target config.bin by default unless otherwise told not to
  want $ if not (null targets) then targets else [ bld "config.bin" ]

  --
  -- primary build rules
  --

  bld "config.bin" %> \out -> icemulti out apps

  apps |%> \out -> do
    need [ out -<.> "timing.rpt" ]
    icepack (out -<.> "asc") out

  bld "//*.timing.rpt" %> \out -> icetime (dropExtensions out -<.> "asc") out

  bld "//*.asc" %> \out -> arachne (dropExtensions out -<.> "blif") out

  bld "//*.blif" %> \out -> yosys (dropExtensions out -<.> "ys")

  bld "//*.ys" %> \out -> do
    need [ mainhi ]
    let blif = out -<.> "blif"
        app  = takeBaseName out
        dir  = rtldir </> app
        file = dir </> app -<.> "manifest"
    manifest <- fmap readMaybe $ readFile' file

    case manifest of
      Nothing -> fail $ "Couldn't read Manifest file " <> file
      Just man -> do
        let comps = map T.unpack $ componentNames man
            files = [ dir </> c -<.> "v" | c <- comps ]
        need files
        writeFileLines out
          $ map ("read_verilog " <>) files
         ++ [ "synth_ice40 -top " <> app <> " -blif " <> blif ]

  mainhi %> \_ -> clash top

  --
  -- helpful phony targets
  --

  "clean" ~> removeFilesAfter bdir ["//*"]

  "program" ~> do
    need [ bld "config.bin" ]
    putNormal "Executing `iceprog` as root!"
    cmd_ "sudo iceprog config.bin"
