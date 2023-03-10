{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import Lexer
import Frontend.Syntax
import Control.Monad.Except
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

newtype HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12 = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn5 :: t5 -> (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12)
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12) -> t5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: t6 -> (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12)
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12) -> t6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: t7 -> (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12)
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12) -> t7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: t8 -> (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12)
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12) -> t8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: t9 -> (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12)
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12) -> t9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: t10 -> (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12)
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12) -> t10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: t11 -> (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12)
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12) -> t11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: t12 -> (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12)
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12) -> t12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyInTok :: (Token) -> (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12)
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn t5 t6 t7 t8 t9 t10 t11 t12) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\xf0\xf4\x00\x00\x0a\x01\xc0\xd3\x03\x00\x28\x04\x00\x4f\x0f\x00\xa0\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\x7f\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\xd3\x03\x00\x08\x04\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x10\x00\x3c\x3d\x00\x80\x40\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x3d\x00\x80\x42\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x10\x00\x00\x00\x00\x00\x01\x00\xf0\xf4\xff\x1f\x02\x01\xc0\xd3\x03\x00\x08\x04\x00\x4f\x0f\x00\x20\x10\x00\x00\x00\x00\x00\x40\x00\xf0\xf6\x00\x00\x02\x01\x00\x04\xfc\x7f\x00\x00\x00\x00\x00\x00\x00\x10\x00\x3c\x3d\x00\x80\x40\x00\xf0\xf4\x00\x00\x02\x01\xc0\xd3\x03\x00\x08\x04\x00\x4f\x0f\x00\x20\x10\x00\x3c\x3d\x00\x80\x40\x00\xf0\xf4\x00\x00\x02\x01\xc0\xd3\x03\x00\x08\x04\x00\x4f\x0f\x00\x20\x10\x00\x3c\x3d\x00\x80\x40\x00\xf0\xf4\x00\x00\x02\x01\xc0\xd3\x03\x00\x08\x04\x00\x4f\x0f\x00\x20\x10\x00\x3c\x3d\x00\x80\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\x01\x00\x00\x00\x00\xc0\x07\x00\x00\x00\x00\x00\x1f\x00\x00\x00\x00\x00\x7c\x00\x00\x00\x00\x00\xf0\x01\x00\x00\x00\x00\xc0\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\x00\x00\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x02\x00\x00\x3c\x3d\x00\x80\x40\x00\x00\x00\x00\x00\x40\x00\x00\x00\xfc\x7f\x02\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x01\x00\x4f\xff\xff\x21\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x40\x00\x00\xc0\xd3\x03\x00\x08\x04\x00\x00\x00\x00\x40\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\xd3\x03\x00\x08\x04\x00\x4f\x0f\x00\x20\x10\x00\x3c\x3d\x00\x80\x40\x00\x00\x00\x00\x00\x00\x00\x00\x08\xfc\x7f\x00\x00\x00\x4f\x0f\x00\x20\x10\x00\x3c\x3d\x00\x80\x40\x00\x00\x00\xff\x07\x00\x00\x00\x00\xfc\x7f\x00\x04\x00\x4f\x0f\x00\x20\x10\x00\xbc\xfd\xff\x87\x40\x00\x00\x00\xff\x1f\x00\x00\x00\x00\xfc\x1f\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\xff\x1f\x10\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x0f\x00\x20\x10\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x3d\x00\x80\x40\x00\x00\x00\xff\x1f\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf0\xff\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_sentences","%start_sentences","Sentences","Sentence","Definition","variables","Bindings","CondBindings","Expressions","Expression","let","True","False","if","then","else","cond","define","NUM","lambda","begin","set","'+'","'-'","'*'","'/'","'%'","'=='","'!='","'<'","'>'","'<='","'>='","'&&'","'||'","'='","':='","'=>'","';'","'('","')'","'{'","'}'","'['","']'","'\"'","VAR","%eof"]
        bit_start = st Prelude.* 50
        bit_end = (st Prelude.+ 1) Prelude.* 50
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..49]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x5f\x00\x5f\x00\x5f\x00\x00\x00\x00\x00\x12\x01\xe4\xff\x00\x00\x00\x00\x77\x00\xe7\xff\x00\x00\x0b\x00\x1e\x00\x17\x00\x77\x00\x53\x00\x00\x00\x22\x00\x5f\x00\x22\x00\x00\x00\x20\x00\x3e\x00\x01\x00\x77\x00\x77\x00\x3a\x00\x6b\x00\xa9\x00\x3f\x00\x77\x00\x77\x00\x77\x00\x77\x00\x77\x00\x77\x00\x77\x00\x77\x00\x77\x00\x77\x00\x77\x00\x77\x00\x77\x00\x12\x01\x12\x01\x7e\x00\x7e\x00\x7e\x00\x7e\x00\x7e\x00\x7e\x00\x05\x00\x05\x00\x05\x00\x82\x00\x82\x00\x44\x00\x56\x00\x77\x00\x50\x00\x02\x01\x60\x00\x75\x00\x65\x00\x8a\x00\x40\x00\x05\x00\x00\x00\x00\x00\x00\x00\x8b\x00\x94\x00\x77\x00\xe2\xff\x00\x00\x00\x00\x77\x00\x77\x00\x77\x00\x00\x00\xf5\x00\x77\x00\x77\x00\x12\x01\x90\x00\x77\x00\x21\x00\x12\x01\x12\x01\x95\x00\xb5\x00\xb6\x00\x00\x00\x77\x00\xb7\x00\x00\x00\x05\x00\x00\x00\x77\x00\xcb\x00\x00\x00\xe0\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xaa\x00\x2c\x01\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xca\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x00\x00\x00\x00\x2f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x81\x00\xcc\x00\x92\x00\xd1\x00\x36\x00\x00\x00\xd2\x00\xce\x00\xde\x00\xdf\x00\xe1\x00\xe2\x00\xe3\x00\xe4\x00\xf3\x00\xf6\x00\xf7\x00\xf8\x00\xf9\x00\x15\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x16\x01\x00\x00\x00\x00\x00\x00\x00\x00\x31\x01\x00\x00\xac\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x01\x32\x01\x00\x00\x00\x00\x30\x01\x33\x01\x34\x01\x00\x00\x00\x00\x35\x01\x36\x01\x00\x00\x3a\x01\x38\x01\x55\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x39\x01\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\xfb\xff\xfa\xff\x00\x00\xec\xff\xeb\xff\x00\x00\x00\x00\xea\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe9\xff\x00\x00\xfd\xff\x00\x00\xfc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\xd7\xff\xd8\xff\xd9\xff\xda\xff\xdb\xff\xdc\xff\xdd\xff\xde\xff\xdf\xff\xe0\xff\xe1\xff\xe2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\xff\x00\x00\xef\xff\xd5\xff\xee\xff\xed\xff\xe3\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe6\xff\xf5\xff\x00\x00\x00\x00\x00\x00\xe4\xff\x00\x00\x00\x00\x00\x00\xe5\xff\xf4\xff\x00\x00\xf2\xff\xf0\xff\xe8\xff\x00\x00\x00\x00\x00\x00\xf9\xff\x00\x00\x00\x00\xf1\xff\xe7\xff\xf3\xff\x00\x00\x00\x00\xf8\xff\x00\x00\xf7\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x1f\x00\x01\x00\x02\x00\x03\x00\x04\x00\x22\x00\x25\x00\x07\x00\x22\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x01\x00\x02\x00\x18\x00\x19\x00\x1e\x00\x1f\x00\x07\x00\x01\x00\x02\x00\x03\x00\x04\x00\x25\x00\x06\x00\x07\x00\x1e\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x05\x00\x25\x00\x07\x00\x1e\x00\x1e\x00\x22\x00\x01\x00\x02\x00\x03\x00\x04\x00\x25\x00\x25\x00\x07\x00\x26\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x05\x00\x08\x00\x07\x00\x1f\x00\x1e\x00\x25\x00\x01\x00\x02\x00\x03\x00\x04\x00\x25\x00\x25\x00\x07\x00\x23\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x01\x00\x02\x00\x03\x00\x04\x00\x1a\x00\x06\x00\x07\x00\x23\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x01\x00\x02\x00\x03\x00\x04\x00\x1c\x00\x1e\x00\x07\x00\x20\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x25\x00\x06\x00\x07\x00\x06\x00\x07\x00\x1e\x00\x25\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x25\x00\x0f\x00\x10\x00\x11\x00\x1f\x00\x1e\x00\x18\x00\x19\x00\x06\x00\x07\x00\x18\x00\x19\x00\x25\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x00\x00\x01\x00\x02\x00\x23\x00\x05\x00\x1b\x00\x25\x00\x07\x00\x06\x00\x07\x00\x1f\x00\x25\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1b\x00\x07\x00\x1b\x00\x07\x00\x03\x00\x07\x00\x04\x00\x21\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x07\x00\x07\x00\xff\xff\x07\x00\x07\x00\x07\x00\x07\x00\x21\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x07\x00\x06\x00\xff\xff\x07\x00\x07\x00\x07\x00\x07\x00\x21\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x07\x00\x07\x00\x1c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x00\x00\x01\x00\x02\x00\x00\x00\x01\x00\x02\x00\x07\x00\x07\x00\x03\x00\x03\x00\x07\x00\x07\x00\xff\xff\xff\xff\x07\x00\x07\x00\x07\x00\x07\x00\x04\x00\x07\x00\x07\x00\xff\xff\x07\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x5c\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x1f\x00\x41\x00\x0b\x00\x1d\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x03\x00\x04\x00\x2b\x00\x2c\x00\x10\x00\x46\x00\x05\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x12\x00\x3f\x00\x0b\x00\x1c\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x3c\x00\x1a\x00\x3d\x00\x48\x00\x10\x00\x1b\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x49\x00\x12\x00\x0b\x00\xff\xff\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x60\x00\x17\x00\x3d\x00\x47\x00\x10\x00\x41\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x3b\x00\x12\x00\x0b\x00\x54\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x53\x00\x3f\x00\x0b\x00\x51\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x4f\x00\x10\x00\x0b\x00\x11\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x12\x00\x17\x00\x18\x00\x44\x00\x42\x00\x10\x00\x41\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x12\x00\x22\x00\x23\x00\x24\x00\x4e\x00\x10\x00\x2b\x00\x2c\x00\x41\x00\x42\x00\x2b\x00\x2c\x00\x12\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x14\x00\x13\x00\x04\x00\x4c\x00\x3c\x00\x4a\x00\x4b\x00\x05\x00\x44\x00\x42\x00\x60\x00\x3b\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x5f\x00\x1d\x00\x64\x00\x43\x00\x3f\x00\x38\x00\x39\x00\x5e\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x37\x00\x36\x00\x00\x00\x35\x00\x34\x00\x33\x00\x32\x00\x66\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x31\x00\x57\x00\x00\x00\x30\x00\x2f\x00\x2e\x00\x2d\x00\x68\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2c\x00\x51\x00\x50\x00\x20\x00\x21\x00\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x12\x00\x13\x00\x04\x00\x15\x00\x13\x00\x04\x00\x5c\x00\x05\x00\x4c\x00\x5a\x00\x05\x00\x59\x00\x00\x00\x00\x00\x58\x00\x57\x00\x55\x00\x54\x00\x62\x00\x61\x00\x64\x00\x00\x00\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (2, 42) [
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42)
	]

happy_n_terms = 39 :: Prelude.Int
happy_n_nonterms = 8 :: Prelude.Int

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_2 = happySpecReduce_1  0# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 ([happy_var_1]
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_3 = happySpecReduce_2  0# happyReduction_3
happyReduction_3 happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (happy_var_1 : happy_var_2
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_4 = happySpecReduce_1  1# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOut7 happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (Definition happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_5 = happySpecReduce_1  1# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (Expression happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_6 = happyReduce 6# 2# happyReduction_6
happyReduction_6 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (TokenSym happy_var_3) -> 
	case happyOut12 happy_x_5 of { happy_var_5 -> 
	happyIn7
		 (ExprDefine happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_7 = happyReduce 8# 2# happyReduction_7
happyReduction_7 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_4 of { (TokenSym happy_var_4) -> 
	case happyOut12 happy_x_7 of { happy_var_7 -> 
	happyIn7
		 (FuncDefine happy_var_4 [] happy_var_7
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_8 = happyReduce 9# 2# happyReduction_8
happyReduction_8 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_4 of { (TokenSym happy_var_4) -> 
	case happyOut8 happy_x_5 of { happy_var_5 -> 
	case happyOut12 happy_x_8 of { happy_var_8 -> 
	happyIn7
		 (FuncDefine happy_var_4 happy_var_5 happy_var_8
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_9 = happySpecReduce_1  3# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenSym happy_var_1) -> 
	happyIn8
		 ([happy_var_1]
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_10 = happySpecReduce_2  3# happyReduction_10
happyReduction_10 happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenSym happy_var_1) -> 
	case happyOut8 happy_x_2 of { happy_var_2 -> 
	happyIn8
		 (happy_var_1 : happy_var_2
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_11 = happySpecReduce_3  4# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenSym happy_var_1) -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 ([(happy_var_1, happy_var_3)]
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_12 = happyReduce 4# 4# happyReduction_12
happyReduction_12 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_1 of { (TokenSym happy_var_1) -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	case happyOut9 happy_x_4 of { happy_var_4 -> 
	happyIn9
		 ((happy_var_1, happy_var_3) : happy_var_4
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_13 = happySpecReduce_3  5# happyReduction_13
happyReduction_13 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 ([(happy_var_1, happy_var_3)]
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_14 = happyReduce 4# 5# happyReduction_14
happyReduction_14 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	case happyOut10 happy_x_4 of { happy_var_4 -> 
	happyIn10
		 ((happy_var_1, happy_var_3) : happy_var_4
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_15 = happySpecReduce_3  5# happyReduction_15
happyReduction_15 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn10
		 ([(ExprVar "else", happy_var_3)]
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_16 = happySpecReduce_1  6# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 ([happy_var_1]
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_17 = happySpecReduce_2  6# happyReduction_17
happyReduction_17 happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut11 happy_x_2 of { happy_var_2 -> 
	happyIn11
		 (happy_var_1 : happy_var_2
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_18 = happySpecReduce_3  7# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn12
		 (happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_19 = happySpecReduce_1  7# happyReduction_19
happyReduction_19 happy_x_1
	 =  happyIn12
		 (ExprInt 1
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_20 = happySpecReduce_1  7# happyReduction_20
happyReduction_20 happy_x_1
	 =  happyIn12
		 (ExprInt 0
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_21 = happySpecReduce_1  7# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenNum happy_var_1) -> 
	happyIn12
		 (ExprInt happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_22 = happySpecReduce_1  7# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TokenSym happy_var_1) -> 
	happyIn12
		 (ExprVar happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_23 = happyReduce 5# 7# happyReduction_23
happyReduction_23 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_3 of { happy_var_3 -> 
	case happyOut12 happy_x_5 of { happy_var_5 -> 
	happyIn12
		 (ExprLambda happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_24 = happyReduce 6# 7# happyReduction_24
happyReduction_24 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_2 of { happy_var_2 -> 
	case happyOut12 happy_x_4 of { happy_var_4 -> 
	case happyOut12 happy_x_6 of { happy_var_6 -> 
	happyIn12
		 (ExprIf happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_25 = happyReduce 4# 7# happyReduction_25
happyReduction_25 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut11 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprBegin happy_var_3
	) `HappyStk` happyRest}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_26 = happyReduce 5# 7# happyReduction_26
happyReduction_26 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_3 of { happy_var_3 -> 
	case happyOut12 happy_x_5 of { happy_var_5 -> 
	happyIn12
		 (ExprLet happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_27 = happyReduce 4# 7# happyReduction_27
happyReduction_27 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprCond happy_var_3
	) `HappyStk` happyRest}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_28 = happySpecReduce_3  7# happyReduction_28
happyReduction_28 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_2 of { happy_var_2 -> 
	happyIn12
		 (ExprApp happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_29 = happySpecReduce_3  7# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprAdd happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_30 = happySpecReduce_3  7# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprSub happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_31 = happySpecReduce_3  7# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprMul happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_32 = happySpecReduce_3  7# happyReduction_32
happyReduction_32 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprDiv happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_33 = happySpecReduce_3  7# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprMod happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_34 = happySpecReduce_3  7# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprEq happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_35 = happySpecReduce_3  7# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprNeq happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_36 = happySpecReduce_3  7# happyReduction_36
happyReduction_36 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprLt happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_37 = happySpecReduce_3  7# happyReduction_37
happyReduction_37 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprGt happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_38 = happySpecReduce_3  7# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprLe happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_39 = happySpecReduce_3  7# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprGe happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_40 = happySpecReduce_3  7# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprAnd happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_41 = happySpecReduce_3  7# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprOr happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_42 = happySpecReduce_3  7# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_2 of { (TokenSym happy_var_2) -> 
	case happyOut12 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (ExprSet happy_var_2 happy_var_3
	)}}

happyNewToken action sts stk [] =
	happyDoAction 38# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	TokenLet -> cont 1#;
	TokenTrue -> cont 2#;
	TokenFalse -> cont 3#;
	TokenIf -> cont 4#;
	TokenThen -> cont 5#;
	TokenElse -> cont 6#;
	TokenCond -> cont 7#;
	TokenDefine -> cont 8#;
	TokenNum happy_dollar_dollar -> cont 9#;
	TokenLambda -> cont 10#;
	TokenBegin -> cont 11#;
	TokenSet -> cont 12#;
	TokenAdd -> cont 13#;
	TokenSub -> cont 14#;
	TokenMul -> cont 15#;
	TokenDiv -> cont 16#;
	TokenMod -> cont 17#;
	TokenEq -> cont 18#;
	TokenNeq -> cont 19#;
	TokenLt -> cont 20#;
	TokenGt -> cont 21#;
	TokenLe -> cont 22#;
	TokenGe -> cont 23#;
	TokenAnd -> cont 24#;
	TokenOr -> cont 25#;
	TokenAss -> cont 26#;
	TokenColEq -> cont 27#;
	TokenRightarrow -> cont 28#;
	TokenSemi -> cont 29#;
	TokenLParen -> cont 30#;
	TokenRParen -> cont 31#;
	TokenLBrace -> cont 32#;
	TokenRBrace -> cont 33#;
	TokenLBracket -> cont 34#;
	TokenRBracket -> cont 35#;
	TokenQuote -> cont 36#;
	TokenSym happy_dollar_dollar -> cont 37#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 38# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Except String a -> (a -> Except String b) -> Except String b
happyThen = ((>>=))
happyReturn :: () => a -> Except String a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Except String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Except String a
happyError' = (\(tokens, _) -> parseError tokens)
sentences tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {x' = happyOut5 x} in x'))

sentences tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {x' = happyOut5 x} in x'))

happySeq = happyDontSeq


parseError :: [Token] -> Except String a
parseError (l : ls) = throwError (show (l : ls))
parseError [] = throwError "Unexpected end of Input"

parse :: String -> Either String Sentences
parse input = runExcept $ do
    tokenStream <- scanTokens input
    sentences tokenStream
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
