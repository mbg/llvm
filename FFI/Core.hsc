{--------------------------------------------------------------------------------------------------
                                           Cada Compiler                                           
                                       Michael Benjamin Gale                                       
--------------------------------------------------------------------------------------------------}

{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, DeriveDataTypeable #-}

module LLVM.FFI.Core(
    ContextRef(..),
    ModuleRef(..),
    TypeRef(..),
    ValueRef(..),
    BuilderRef(..),
    BasicBlockRef(..),
    
    Visibility(..),
    fromVisibility,
    toVisibility,
    
    Linkage(..),
    fromLinkage,
    llvmCreateContext,
    
    CallingConv(..),
    fromCallingConv,
    
    Attribute(..),
    fromAttribute,
    
    IntPred(..),
    fromIntPred,
    
    llvmGetGlobalContext,
    llvmDisposeContext,
    llvmCreateModuleWithName,
    llvmSetModuleInlineAsm,
    llvmDumpModule,
    llvmWriteModuleToFile,
    llvmDisposeModule,
    
    llvmDumpValue,
    llvmSetValueName,
    
    llvmTypeOf,
    llvmVoidType,
    llvmInt1Type,
    llvmInt8Type,
    llvmInt16Type,
    llvmInt32Type,
    llvmInt64Type,
    llvmIntType,
    llvmGlInt8Type,
    llvmArrayType,
    llvmPointerType,
    llvmFloatType,
    llvmDoubleType,
    llvmCreateFunctionType,
    llvmCreateStructType,
    llvmCreateNamedStruct,
    
    llvmConstAdd,
    llvmConstSub,
    llvmConstPtrToInt,
    llvmConstGEP,
    llvmConstInBoundsGEP,
    
    llvmSetStructBody,
    llvmConstInt,
    llvmConstArray,
    llvmConstNamedStruct,
    llvmConstNull,
    
    llvmConstString,
    
    llvmAddFunction,
    llvmGetNamedFunction,
    llvmAddFunctionAttr,
    
    llvmSetLinkage,
    llvmSetVisibility,
    llvmGetParam,
    
    llvmAppendBasicBlock,
    
    llvmSetCallConv,
    llvmAddAttribute,
    llvmSetTailCall,
    
    llvmAddGlobal,
    llvmSetGlobalConstant,
    llvmSetInitialiser,
    
    llvmCreateBuilder,
    llvmPositionBuilderAtEnd,
    llvmDisposeBuilder,
    
    llvmBuildRetVoid,
    llvmBuildBr,
    llvmBuildCondBr,
    
    llvmBuildAdd,
    llvmBuildSub,
    llvmBuildMul,
    llvmBuildUDiv,
    llvmBuildAnd,
    llvmBuildOr,
    llvmBuildNeg,
    llvmBuildNot,
    
    llvmBuildICmp,
    
    llvmBuildAlloca,
    llvmBuildLoad,
    llvmBuildStore,
    llvmBuildGEP,
    llvmBuildInBoundsGEP,
    
    llvmBuildTrunc,
    llvmBuildZExt,
    llvmBuildFPtoUI,
    llvmBuildFPtoSI,
    llvmBuildUItoFP,
    llvmBuildSItoFP,
    
    llvmBuildGlobalString,
    llvmBuildGlobalStringPtr,
    
    llvmBuildBitCast,
    llvmBuildPtrToInt,
    llvmBuildIntToPtr,
    
    llvmBuildCall
) where

#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

import Data.Typeable(Typeable)
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..), CInt(..), CUInt(..), CLLong(..), CULong(..))
import Foreign.Ptr (Ptr, FunPtr)

#include <llvm-c/Core.h>
#include <llvm-c/BitWriter.h>

data Context deriving (Typeable)
data Module deriving (Typeable)
data Type deriving (Typeable)
data Value deriving (Typeable)
data Builder deriving (Typeable)
data BasicBlock deriving (Typeable)

type ContextRef    = Ptr Context
type ModuleRef     = Ptr Module
type TypeRef       = Ptr Type
type ValueRef      = Ptr Value
type BuilderRef    = Ptr Builder
type BasicBlockRef = Ptr BasicBlock

data Visibility = Visible
                | Hidden
                | Protected
                
fromVisibility :: Visibility -> CUInt
fromVisibility Visible   = (#const LLVMDefaultVisibility)
fromVisibility Hidden    = (#const LLVMHiddenVisibility)
fromVisibility Protected = (#const LLVMProtectedVisibility)

toVisibility :: CUInt -> Visibility
toVisibility c | c == (#const LLVMDefaultVisibility)   = Visible
toVisibility c | c == (#const LLVMHiddenVisibility)    = Hidden
toVisibility c | c == (#const LLVMProtectedVisibility) = Protected

data Linkage = ExternalLinkage
             | AvailableExternallyLinkage
             | LinkOnceAnyLinkage
             | LinkOnceODRLinkage
             | WeakAnyLinkage
             | WeakODRLinkage
             | AppendingLinkage
             | InternalLinkage
             | PrivateLinkage
             | DLLImportLinkage
             | DLLExportLinkage
             | ExternalWeakLinkage
             | GhostLinkage
             | CommonLinkage
             | LinkerPrivateLinkage
             | LinkerPrivateWeakLinkage
             | LinkerPrivateWeakDefAutoLinkage
             
fromLinkage :: Linkage -> CUInt
fromLinkage InternalLinkage = (#const LLVMInternalLinkage)
fromLinkage CommonLinkage   = (#const LLVMCommonLinkage)
fromLinkage PrivateLinkage  = (#const LLVMPrivateLinkage)

data CallingConv = C
                 | GHC
                 
fromCallingConv :: CallingConv -> CUInt
fromCallingConv C   = (#const LLVMCCallConv)
fromCallingConv GHC = 10

data Attribute = ZExt
               | SExt
               | NoReturn
               | InReg
               | StructRet
               | NoUnwind
               | NoAlias
               | ByVal
               | Nest
               | ReadNone
               | ReadOnly
               | NoInline
               | AlwaysInline
               | OptimizeForSize
               | StackProtect
               | StackProtectReq
               | Alignment
               | NoCapture
               | NoRedZone
               | NoImplicitFloat
               | Naked
               | InlineHint
               | StackAlignment
               | ReturnsTwice
               | UWTable
               | NonLazyBind
               
fromAttribute :: Attribute -> CUInt
fromAttribute NoUnwind = (#const LLVMNoUnwindAttribute)

data IntPred = IntEQ
             | IntNE
             | IntUGT
             | IntUGE
             | IntULT
             | IntULE
             | IntSGT
             | IntSGE
             | IntSLT
             | IntSLE

fromIntPred :: IntPred -> CUInt
fromIntPred IntEQ  = (#const LLVMIntEQ)
fromIntPred IntNE  = (#const LLVMIntNE)
fromIntPred IntUGT = (#const LLVMIntUGT)
fromIntPred IntUGE = (#const LLVMIntUGE)
fromIntPred IntULT = (#const LLVMIntULT)
fromIntPred IntULE = (#const LLVMIntULE)
fromIntPred IntSGT = (#const LLVMIntSGT)
fromIntPred IntSGE = (#const LLVMIntSGE)
fromIntPred IntSLT = (#const LLVMIntSLT)
fromIntPred IntSLE = (#const LLVMIntSLE)
             
foreign import ccall unsafe "LLVMContextCreate" llvmCreateContext :: IO ContextRef  
foreign import ccall unsafe "LLVMGetGlobalContext" llvmGetGlobalContext :: IO ContextRef 
foreign import ccall unsafe "LLVMContextDispose" llvmDisposeContext :: ContextRef -> IO ()
 
foreign import ccall unsafe "LLVMModuleCreateWithName" llvmCreateModuleWithName :: CString -> IO ModuleRef  
foreign import ccall unsafe "LLVMSetModuleInlineAsm" llvmSetModuleInlineAsm :: ModuleRef -> CString -> IO ()  
foreign import ccall unsafe "LLVMDumpModule" llvmDumpModule :: ModuleRef -> IO ()
foreign import ccall unsafe "LLVMWriteBitcodeToFile" llvmWriteModuleToFile :: ModuleRef -> CString -> IO ()
foreign import ccall unsafe "LLVMDisposeModule" llvmDisposeModule :: ModuleRef -> IO ()

foreign import ccall unsafe "LLVMDumpValue" llvmDumpValue :: ValueRef -> IO ()
foreign import ccall unsafe "LLVMSetValueName" llvmSetValueName :: ValueRef -> CString -> IO ()

foreign import ccall unsafe "LLVMTypeOf" llvmTypeOf :: ValueRef -> IO TypeRef
foreign import ccall unsafe "LLVMVoidType" llvmVoidType :: IO TypeRef

foreign import ccall unsafe "LLVMInt1TypeInContext" llvmInt1Type :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMInt8TypeInContext" llvmInt8Type :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMInt16TypeInContext" llvmInt16Type :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMInt32TypeInContext" llvmInt32Type :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMInt64TypeInContext" llvmInt64Type :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMIntTypeInContext" llvmIntType :: ContextRef -> CUInt -> IO TypeRef
foreign import ccall unsafe "LLVMInt8Type" llvmGlInt8Type :: IO TypeRef
foreign import ccall unsafe "LLVMArrayType" llvmArrayType :: TypeRef -> CUInt -> IO TypeRef
foreign import ccall unsafe "LLVMPointerType" llvmPointerType :: TypeRef -> CUInt -> IO TypeRef

foreign import ccall unsafe "LLVMFloatTypeInContext" llvmFloatType :: ContextRef -> IO TypeRef
foreign import ccall unsafe "LLVMDoubleTypeInContext" llvmDoubleType :: ContextRef -> IO TypeRef

foreign import ccall unsafe "LLVMFunctionType" llvmCreateFunctionType :: TypeRef -> Ptr TypeRef -> CUInt -> CInt -> IO TypeRef

foreign import ccall unsafe "LLVMStructType" llvmCreateStructType :: Ptr TypeRef -> CUInt -> CInt -> IO TypeRef
foreign import ccall unsafe "LLVMStructCreateNamed" llvmCreateNamedStruct :: ContextRef -> CString -> IO TypeRef
foreign import ccall unsafe "LLVMStructSetBody" llvmSetStructBody :: TypeRef -> Ptr TypeRef -> CUInt -> CInt -> IO ()

foreign import ccall unsafe "LLVMConstInt" llvmConstInt :: TypeRef -> CULong -> Int -> IO ValueRef
foreign import ccall unsafe "LLVMConstArray" llvmConstArray :: TypeRef -> Ptr ValueRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstStruct" llvmConstStruct :: Ptr ValueRef -> CUInt -> Int -> IO ValueRef
foreign import ccall unsafe "LLVMConstNamedStruct" llvmConstNamedStruct :: TypeRef -> Ptr ValueRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstNull" llvmConstNull :: TypeRef -> IO ValueRef

foreign import ccall unsafe "LLVMConstString" llvmConstString :: CString -> CUInt -> Int -> IO ValueRef

foreign import ccall unsafe "LLVMConstAdd" llvmConstAdd :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstSub" llvmConstSub :: ValueRef -> ValueRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstPtrToInt" llvmConstPtrToInt :: ValueRef -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMConstGEP" llvmConstGEP :: ValueRef -> Ptr ValueRef -> CUInt -> IO ValueRef
foreign import ccall unsafe "LLVMConstInBoundsGEP" llvmConstInBoundsGEP :: ValueRef -> Ptr ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMAddFunction" llvmAddFunction :: ModuleRef -> CString -> TypeRef -> IO ValueRef
foreign import ccall unsafe "LLVMGetNamedFunction" llvmGetNamedFunction :: ModuleRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMAddFunctionAttr" llvmAddFunctionAttr :: ValueRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMSetLinkage" llvmSetLinkage :: ValueRef -> CUInt -> IO ()
foreign import ccall unsafe "LLVMSetVisibility" llvmSetVisibility :: ValueRef -> CUInt -> IO ()

foreign import ccall unsafe "LLVMGetParam" llvmGetParam :: ValueRef -> CUInt -> IO ValueRef

foreign import ccall unsafe "LLVMAppendBasicBlock" llvmAppendBasicBlock :: ValueRef -> CString -> IO BasicBlockRef
foreign import ccall unsafe "LLVMCountBasicBlocks" llvmCountBasicBlocks :: ValueRef -> IO CUInt
foreign import ccall unsafe "LLVMGetBasicBlocks" llvmGetBasicBlocks :: ValueRef -> Ptr BasicBlockRef -> IO ()

foreign import ccall unsafe "LLVMSetInstructionCallConv" llvmSetCallConv :: ValueRef -> CUInt -> IO ()
foreign import ccall unsafe "LLVMAddInstrAttribute" llvmAddAttribute :: ValueRef -> CUInt -> CUInt -> IO ()
foreign import ccall unsafe "LLVMSetTailCall" llvmSetTailCall :: ValueRef -> Int -> IO ()

foreign import ccall unsafe "LLVMAddGlobal" llvmAddGlobal :: ModuleRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMSetGlobalConstant" llvmSetGlobalConstant :: ValueRef -> CInt -> IO ()
foreign import ccall unsafe "LLVMSetInitializer" llvmSetInitialiser :: ValueRef -> ValueRef -> IO ()

foreign import ccall unsafe "LLVMCreateBuilder" llvmCreateBuilder :: IO BuilderRef
foreign import ccall unsafe "LLVMPositionBuilderAtEnd" llvmPositionBuilderAtEnd :: BuilderRef -> BasicBlockRef -> IO ()
foreign import ccall unsafe "LLVMDisposeBuilder" llvmDisposeBuilder :: BuilderRef -> IO ()

foreign import ccall unsafe "LLVMBuildRetVoid" llvmBuildRetVoid :: BuilderRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildBr" llvmBuildBr :: BuilderRef -> BasicBlockRef -> IO ValueRef
foreign import ccall unsafe "LLVMBuildCondBr" llvmBuildCondBr :: BuilderRef -> ValueRef -> BasicBlockRef -> BasicBlockRef -> IO ValueRef

foreign import ccall unsafe "LLVMBuildAdd" llvmBuildAdd :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSub" llvmBuildSub :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildMul" llvmBuildMul :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildUDiv" llvmBuildUDiv :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildAnd" llvmBuildAnd :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildOr" llvmBuildOr :: BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNeg" llvmBuildNeg :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildNot" llvmBuildNot :: BuilderRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildICmp" llvmBuildICmp :: BuilderRef -> CUInt -> ValueRef -> ValueRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildAlloca" llvmBuildAlloca :: BuilderRef -> TypeRef -> CString -> IO ValueRef 
foreign import ccall unsafe "LLVMBuildLoad" llvmBuildLoad :: BuilderRef -> ValueRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildStore" llvmBuildStore :: BuilderRef -> ValueRef -> ValueRef -> IO ValueRef 
foreign import ccall unsafe "LLVMBuildGEP" llvmBuildGEP :: BuilderRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildInBoundsGEP" llvmBuildInBoundsGEP :: BuilderRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildTrunc" llvmBuildTrunc :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildZExt" llvmBuildZExt :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFPToUI" llvmBuildFPtoUI :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildFPToSI" llvmBuildFPtoSI :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildUIToFP" llvmBuildUItoFP :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildSIToFP" llvmBuildSItoFP :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

                             
foreign import ccall unsafe "LLVMBuildGlobalString" llvmBuildGlobalString :: BuilderRef -> CString -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildGlobalStringPtr" llvmBuildGlobalStringPtr :: BuilderRef -> CString -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildBitCast" llvmBuildBitCast :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildPtrToInt" llvmBuildPtrToInt :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef
foreign import ccall unsafe "LLVMBuildIntToPtr" llvmBuildIntToPtr :: BuilderRef -> ValueRef -> TypeRef -> CString -> IO ValueRef

foreign import ccall unsafe "LLVMBuildCall" llvmBuildCall :: BuilderRef -> ValueRef -> Ptr ValueRef -> CUInt -> CString -> IO ValueRef

