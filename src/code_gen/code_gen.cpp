#include "compiler/compiler.h"
#include "debug_info/debug_info.h"
#include "error_report.h"
#include "lexer/lexer.h"
#include "lld/Common/Driver.h"
#include "parser/parser.h"
#include "semantic_analyzer/semantic_analyzer.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/OptimizationLevel.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/Alignment.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/VirtualFileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/TargetParser/Triple.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Utils/ValueMapper.h"
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <format>
#include <memory>
#include <string>
static void removeReadonlyPrefix(std::string &str)
{
    const std::string prefix = "readonly ";
    const size_t prefixLen = prefix.length();

    if (str.compare(0, prefixLen, prefix) == 0)
    {
        str.erase(0, prefixLen);
    }
}
IRType CodeGen::ZtoonTypeToLLVMType(DataType *type)
{
    IRType irType = {};
    switch (type->GetType())
    {

    case DataType::Type::I8:
    {
        irType.type = irBuilder->getIntNTy(8);
        irType.isSigned = true;
        break;
    }
    case DataType::Type::I16:
    {
        irType.type = irBuilder->getIntNTy(16);
        irType.isSigned = true;
        break;
    }
    case DataType::Type::I32:
    {
        irType.type = irBuilder->getIntNTy(32);
        irType.isSigned = true;
        break;
    }
    case DataType::Type::I64:
    {
        irType.type = irBuilder->getIntNTy(64);
        irType.isSigned = true;
        break;
    }
    case DataType::Type::U8:
    {
        irType.type = irBuilder->getIntNTy(8);
        break;
    }
    case DataType::Type::U16:
    {
        irType.type = irBuilder->getIntNTy(16);
        break;
    }
    case DataType::Type::U32:
    {
        irType.type = irBuilder->getIntNTy(32);
        break;
    }
    case DataType::Type::U64:
    {
        irType.type = irBuilder->getIntNTy(64);
        break;
    }
    case DataType::Type::F32:
    {
        irType.type = irBuilder->getFloatTy();
        break;
    }
    case DataType::Type::F64:
    {
        irType.type = irBuilder->getDoubleTy();
        break;
    }
    case DataType::Type::BOOL:
    {
        irType.type = irBuilder->getIntNTy(1);
        break;
    }
    case DataType::Type::NOTYPE:
    {
        irType.type = irBuilder->getVoidTy();
        break;
    }
    case DataType::Type::STRUCT:
    {
        auto structZtoonType = dynamic_cast<StructDataType *>(type);

        llvm::StructType *structIRType =
            llvm::StructType::getTypeByName(*ctx, structZtoonType->GetUID());
        // assert(structIRType);
        if (!structIRType)
        {
            structIRType =
                llvm::StructType::create(*ctx, structZtoonType->GetUID());
            std::vector<llvm::Type *> bodyFields;
            for (DataType *fieldType : structZtoonType->fields)
            {
                IRType irFieldType = ZtoonTypeToLLVMType(fieldType);
                bodyFields.push_back(irFieldType.type);
            }
            structIRType->setBody(bodyFields);
        }

        irType.type = structIRType;
    }
    break;
    case DataType::Type::ENUM:
    {
        EnumDataType *enumType = dynamic_cast<EnumDataType *>(type);
        irType = ZtoonTypeToLLVMType(enumType->datatype);
    }
    break;
    case DataType::Type::UNION:
    {
        UnionDataType *unionType = dynamic_cast<UnionDataType *>(type);
        if (!unionType->largestDatatype)
        {
            // Get Largest datatype.
            size_t largestSize = 0;
            for (DataType *fieldType : unionType->fields)
            {
                IRType irFieldType = ZtoonTypeToLLVMType(fieldType);
                size_t sizeInBytes =
                    moduleDataLayout->getTypeSizeInBits(irFieldType.type) / 8;
                if (largestSize < sizeInBytes)
                {
                    unionType->largestDatatype = fieldType;
                    largestSize = sizeInBytes;
                }
            }
        }

        irType = ZtoonTypeToLLVMType(unionType->largestDatatype);
        break;
    }
    case DataType::Type::POINTER:
    {
        auto ptrZtoonType = dynamic_cast<PointerDataType *>(type);
        IRType dataType = ZtoonTypeToLLVMType(ptrZtoonType->dataType);

        irType.type = dataType.type->getPointerTo();

        break;
    }
    case DataType::Type::ARRAY:
    {
        auto arrZtoonType = dynamic_cast<ArrayDataType *>(type);
        auto dataType = arrZtoonType->dataType;
        IRType irDataType = ZtoonTypeToLLVMType(dataType);
        if (arrZtoonType->sizeExpr)
        {
            auto sizeExprArrayDataType = dynamic_cast<ArrayDataType *>(
                semanticAnalyzer.exprToDataTypeMap[arrZtoonType->sizeExpr]);
            if (sizeExprArrayDataType)
            {
                arrZtoonType->size = sizeExprArrayDataType->size;
            }
            else
            {
                IRValue arrSize = GenExpressionIR(arrZtoonType->sizeExpr);
                if (!llvm::isa<llvm::Constant>(arrSize.value))
                {
                    ReportError("Array size must be constant",
                                arrZtoonType->sizeExpr->GetCodeErrString());
                }
                int64_t arrConstSize =
                    llvm::dyn_cast<llvm::ConstantInt>(arrSize.value)
                        ->getSExtValue();
                if (arrConstSize < 0)
                {
                    ReportError("Array size must be positive intiger",
                                arrZtoonType->sizeExpr->GetCodeErrString());
                }
                else if (arrConstSize == 0)
                {
                    ReportError("Array size cannot be zero",
                                arrZtoonType->sizeExpr->GetCodeErrString());
                }

                arrZtoonType->size = arrConstSize;
            }
        }

        irType.type = llvm::ArrayType::get(irDataType.type, arrZtoonType->size);
    }
    break;
    case DataType::Type::FN:
    {
        auto fnPtr = dynamic_cast<FnDataType *>(type);
        llvm::Type *retType = ZtoonTypeToLLVMType(fnPtr->returnDataType).type;

        std::vector<llvm::Type *> fnParams;

        for (DataType *param : fnPtr->GetParameters())
        {
            fnParams.push_back(ZtoonTypeToLLVMType(param).type);
        }

        llvm::FunctionType *fnType =
            llvm::FunctionType::get(retType, fnParams, fnPtr->IsVarArgs());
        irType.type = fnType;
    }
    break;
    default:
    {
        break;
    }
    }

    return irType;
}

void CodeGen::AddIRSymbol(IRSymbol *irSymbol)
{
    assert(irSymbol);

    CodeErrString ces;
    if (auto symbol =
            semanticAnalyzer.currentScope->GetSymbol(irSymbol->GetName(), ces))
    {
        symbolToIRSymobMap[symbol] = irSymbol;
    }
}

IRSymbol *CodeGen::GetIRSymbol(std::string name)
{
    IRSymbol *ret = nullptr;
    CodeErrString ces;
    auto symbol = semanticAnalyzer.currentScope->GetSymbol(name, ces);

    ret = symbolToIRSymobMap[symbol];
    return ret;
}

IRValue CodeGen::CastIntToInt(IRValue value, IRType castType)
{
    IRValue castedValue = {};
    if ((value.type.type == castType.type) &&
        (value.type.isSigned == castType.isSigned))
    {
        return value;
    }

    uint32_t valueBitWidth = value.type.type->getIntegerBitWidth();
    uint32_t castTypeBitWidth = castType.type->getIntegerBitWidth();

    if (valueBitWidth > castTypeBitWidth)
    {
        // truncate value
        castedValue.value = irBuilder->CreateTrunc(value.value, castType.type);
    }
    else if (valueBitWidth < castTypeBitWidth)
    {
        castedValue.value =
            value.type.isSigned
                ? irBuilder->CreateSExt(value.value, castType.type)
                : irBuilder->CreateZExt(value.value, castType.type);
    }
    else
    {
        castedValue.value = value.value;
    }

    castedValue.type = castType;
    return castedValue;
}
IRValue CodeGen::CastFloatToFloat(IRValue value, IRType castType)
{

    IRValue castedValue = {};
    if (value.type.type == castType.type)
    {
        return value;
    }

    uint32_t valueBitWidth = value.type.type->isFloatTy() ? 32 : 64;
    uint32_t castTypeBitWidth = castType.type->isFloatTy() ? 32 : 64;

    if (valueBitWidth > castTypeBitWidth)
    {
        // truncate value
        castedValue.value =
            irBuilder->CreateFPTrunc(value.value, castType.type);
    }
    else
    {
        castedValue.value = irBuilder->CreateFPExt(value.value, castType.type);
    }

    castedValue.type = castType;
    return castedValue;
}
IRValue CodeGen::CastFloatToInt(IRValue value, IRType castType)
{
    IRValue castedValue = {};
    uint32_t valueBitWidth = value.type.type->isFloatTy() ? 32 : 64;
    uint32_t castTypeBitWidth = castType.type->getIntegerBitWidth();
    castedValue.type = castType;
    castedValue.value =
        castType.isSigned ? irBuilder->CreateFPToSI(value.value, castType.type)
                          : irBuilder->CreateFPToUI(value.value, castType.type);
    castedValue = CastIntToInt(castedValue, castType);
    return castedValue;
}
IRValue CodeGen::CastIntToFloat(IRValue value, IRType castType)
{

    IRValue castedValue = {};
    uint32_t valueBitWidth = value.type.type->getIntegerBitWidth();
    uint32_t castTypeBitWidth = castType.type->isFloatTy() ? 32 : 64;
    castedValue.type = castType;
    castedValue.value =
        value.type.isSigned
            ? irBuilder->CreateSIToFP(value.value, castType.type)
            : irBuilder->CreateUIToFP(value.value, castType.type);
    castedValue = CastFloatToFloat(castedValue, castType);
    return castedValue;
}

IRValue CodeGen::CastPtrToPtr(IRValue value, IRType castType)
{
    IRValue irValue = {};
    irValue.type = castType;
    irValue.value = irBuilder->CreateBitCast(value.value, castType.type);

    return irValue;
}
IRValue CodeGen::CastIntToPtr(IRValue value, IRType castType)
{
    IRValue irValue = {};
    // first cast valut int to int with arch bit width.
    IRType intType = {};
    intType.type = llvm::Type::getIntNTy(*ctx, GetPtrBitWidth());
    irValue = CastIntToInt(value, intType);
    irValue.value = irBuilder->CreateIntToPtr(value.value, castType.type);
    irValue.type = castType;

    return irValue;
}
IRValue CodeGen::CastPtrToInt(IRValue value, IRType castType)
{

    IRValue irValue = {};
    irValue.value = irBuilder->CreatePtrToInt(value.value, castType.type);
    irValue.type = castType;

    return irValue;
}
IRValue CodeGen::CastIRValue(IRValue value, IRType castType)
{
    // int to int
    if (value.value->getType()->isIntegerTy() && castType.type->isIntegerTy())
    {
        return CastIntToInt(value, castType);
    }
    // float to float
    else if (value.value->getType()->isFloatingPointTy() &&
             castType.type->isFloatingPointTy())
    {
        return CastFloatToFloat(value, castType);
    }
    // float to int
    else if (value.value->getType()->isFloatingPointTy() &&
             castType.type->isIntegerTy())
    {
        return CastFloatToInt(value, castType);
    }
    else if (value.value->getType()->isIntegerTy() &&
             castType.type->isFloatingPointTy())
    {
        return CastIntToFloat(value, castType);
    }
    // ptr to ptr
    else if (value.value->getType()->isPointerTy() &&
             castType.type->isPointerTy())
    {
        return CastPtrToPtr(value, castType);
    }
    // int to ptr
    else if (value.value->getType()->isIntegerTy() &&
             castType.type->isPointerTy())
    {
        return CastIntToPtr(value, castType);
    }
    // ptr to int
    else if (value.value->getType()->isPointerTy() &&
             castType.type->isIntegerTy())
    {
        return CastPtrToInt(value, castType);
    }
    else
    {
        return IRValue{};
    }
}

CodeGen::CodeGen(SemanticAnalyzer &semanticAnalyzer)
    : semanticAnalyzer(semanticAnalyzer)
{
    currentStage = Stage::CODE_GEN;
}

LLD_HAS_DRIVER(coff)
LLD_HAS_DRIVER(elf)
LLD_HAS_DRIVER(mingw)
LLD_HAS_DRIVER(macho)
LLD_HAS_DRIVER(wasm)

void CodeGen::Compile(Project &project, bool printIR)
{

    this->project = &project;

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();
    llvm::InitializeNativeTarget(); // Critical for host detection
    llvm::InitializeNativeTargetAsmParser();
    llvm::InitializeNativeTargetAsmPrinter();

    ctx = std::make_unique<llvm::LLVMContext>();
    module = std::make_unique<llvm::Module>("ztoon module", *ctx);

    if (project.targetArch.empty())
    {
        project.targetArch = LLVM_HOST_TRIPLE;
    }
    module->setTargetTriple(project.targetArch);
    module->addModuleFlag(llvm::Module::Warning, "Debug Info Version",
                          llvm::DEBUG_METADATA_VERSION);
    moduleDataLayout = std::make_unique<llvm::DataLayout>(module.get());
    irBuilder = std::make_unique<llvm::IRBuilder<>>(*ctx);
    debugInfo = gZtoonArena.Allocate<DebugInfo>(this);

    llvm::Triple triple(module->getTargetTriple());

    if (triple.isOSWindows())
    {
        project.platform = Project::Platform::WINDOWS;
        module->addModuleFlag(llvm::Module::Warning, "CodeView", 1);
    }
    else if (triple.isOSLinux())
    {
        project.platform = Project::Platform::LINUX;
    }
    else if (triple.isMacOSX())
    {
        project.platform = Project::Platform::MACOS;
    }
    else if (triple.isiOS())
    {
        project.platform = Project::Platform::IOS;
    }
    else if (triple.isAndroid())
    {
        project.platform = Project::Platform::ANDROID;
    }
    else if (triple.isWasm())
    {
        project.platform = Project::Platform::WASM;
    }

    std::function<void(Library * lib, bool genTypeSymbol, bool genFnAndVarBody,
                       bool genSymoblBody)>
        GenLibIR;
    GenLibIR = [&](Library *lib, bool genTypeSymbol, bool genFnAndVarBody,
                   bool genSymoblBody)
    {
        GenIR(lib->packages, genTypeSymbol, genFnAndVarBody, genSymoblBody);
        for (auto l : lib->libs)
        {
            GenLibIR(l, genTypeSymbol, genFnAndVarBody, genSymoblBody);
        }
    };
    for (auto lib : semanticAnalyzer.libraries)
    {
        GenLibIR(lib, true, false, false);
    }
    GenIR(semanticAnalyzer.packages, true, false, false);
    for (auto lib : semanticAnalyzer.libraries)
    {
        GenLibIR(lib, false, true, false);
    }
    GenIR(semanticAnalyzer.packages, false, true, false);
    for (auto lib : semanticAnalyzer.libraries)
    {
        GenLibIR(lib, false, false, true);
    }
    GenIR(semanticAnalyzer.packages, false, false, true);
    if (llvm::verifyModule(*module, &llvm::errs()))
    {
        llvm::errs() << "Module verification failed\n";
    }

    debugInfo->Finalize();

    std::string error;
    const llvm::Target *target =
        llvm::TargetRegistry::lookupTarget(module->getTargetTriple(), error);
    if (!target)
    {
        PrintError(std::format("Failed to find target '{}'",
                               module->getTargetTriple()));
    }
    llvm::TargetOptions targetOpts;
    targetOpts.DebuggerTuning = llvm::DebuggerKind::Default;
    std::optional<llvm::Reloc::Model> relocModel = llvm::Reloc::Model::PIC_;
    llvm::TargetMachine *targetMachine = target->createTargetMachine(
        module->getTargetTriple(), "generic", "", targetOpts, relocModel);

    module->setDataLayout(targetMachine->createDataLayout());
    llvm::LoopAnalysisManager LAM;
    llvm::FunctionAnalysisManager FAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;

    llvm::PassBuilder PB;
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
    Project::OptLevel optLevel = project.debugBuild
                                     ? project.debugFlags.optLevel
                                     : project.releaseFlags.optLevel;
    llvm::OptimizationLevel llvmOptLevel;

    switch (optLevel)
    {
    case Project::OptLevel::O0:
    {
        llvmOptLevel = llvm::OptimizationLevel::O0;
    }
    break;
    case Project::OptLevel::O1:
    {
        llvmOptLevel = llvm::OptimizationLevel::O1;
    }
    break;
    case Project::OptLevel::O2:
    {
        llvmOptLevel = llvm::OptimizationLevel::O2;
    }
    break;
    case Project::OptLevel::O3:
    {
        llvmOptLevel = llvm::OptimizationLevel::O3;
    }
    break;
    case Project::OptLevel::OS:
    {
        llvmOptLevel = llvm::OptimizationLevel::Os;
    }
    break;
    case Project::OptLevel::OZ:
    {
        llvmOptLevel = llvm::OptimizationLevel::Oz;
    }
    break;
    }
    llvm::ModulePassManager MPM =
        PB.buildPerModuleDefaultPipeline(llvmOptLevel);
    MPM.run(*module, MAM);

    std::error_code ec;
    std::string objFileName = std::format("bin/{}.o", project.name);
    llvm::raw_fd_ostream objFile(objFileName, ec, llvm::sys::fs::OF_None);
    if (ec)
    {
        PrintError(std::format("Failed to open file: {}", ec.message()));
    }

    llvm::legacy::PassManager pass;
    if (targetMachine->addPassesToEmitFile(pass, objFile, nullptr,
                                           llvm::CodeGenFileType::ObjectFile))
    {
        PrintError("Target machine failed to emit object file");
    }

    if (printIR)
        module->print(llvm::outs(), nullptr);
    pass.run(*module);
    objFile.flush();
}
#ifdef __APPLE__
#include <TargetConditionals.h>
#endif
void CodeGen::Link(Project &project)
{
    std::string objFileName = std::format("bin/{}.o", project.name);

    std::string output = std::format("bin/{}", project.name);

    std::vector<std::string> lldArgs;

    if (project.platform == Project::Platform::WINDOWS)
    {

        for (auto &nl : project.linkerFlags.nativeLibs)
        {
            nl.relative_path = nl.relative_path.generic_string() + ".lib";
        }

        lldArgs.push_back("lld-link");
        if (project.debugBuild)
        {
            lldArgs.push_back("/DEBUG");
            lldArgs.push_back("/ignore:longsections");
        }
        switch (project.type)
        {
        case Project::Type::EXE:
        {
            output = output + ".exe";
        }
        break;
        case Project::Type::ZLIB:
        {
            assert(0);
            break;
        }
        break;
        case Project::Type::STATIC_LIB:
        {
            output = output + ".lib";
            lldArgs.push_back("/lib");
        }
        break;
        case Project::Type::SHARED_LIB:
        {
            output = output + ".dll";
            lldArgs.push_back("/DLL");
            std::string impLib = std::format("/implib:{}.lib", project.name);
            lldArgs.push_back(impLib.c_str());
            if (project.linkerFlags.entry.empty())
            {
                lldArgs.push_back("/entry:DLLMain");
            }
        }
        break;
        }
        lldArgs.push_back(objFileName.c_str());
        std::vector<std::string> libPaths;
        for (auto &libPath : project.linkerFlags.nativeLibs)
        {
            libPaths.push_back(libPath.relative_path.generic_string());
            lldArgs.push_back(libPaths[libPaths.size() - 1].c_str());
        }
        if (!project.linkerFlags.noCRT &&
            project.type != Project::Type::STATIC_LIB)
        {
            switch (project.linkerFlags.crtLinkType)
            {

            case Project::LinkerFlags::CRT_LinkType::STATIC:
                lldArgs.push_back("libcmt.lib");
                break;
            case Project::LinkerFlags::CRT_LinkType::DYNAMIC:
                lldArgs.push_back("msvcrt.lib");
                break;
            }
        }

        std::string out = std::format("/out:{}", output);
        lldArgs.push_back(out.c_str());

        std::string entry = std::format("/entry:{}", project.linkerFlags.entry);
        if (!project.linkerFlags.entry.empty())
        {
            lldArgs.push_back(entry.c_str());
        }
        if (project.type == Project::Type::EXE)
        {
            switch (project.linkerFlags.type)
            {
            case Project::LinkerFlags::Type::CONSOLE:
                lldArgs.push_back("/subsystem:console");
                break;
            case Project::LinkerFlags::Type::WINDOW:
                lldArgs.push_back("/subsystem:windows");
                break;
            }
        }
    }
    else if (project.platform == Project::Platform::MACOS)
    {
        for (auto &nl : project.linkerFlags.nativeLibs)
        {
            nl.relative_path = nl.relative_path.generic_string() + ".a";
        }
        switch (project.type)
        {
        case Project::Type::EXE:
        {
            lldArgs.push_back("ld64.lld");
            lldArgs.push_back("-o");
            lldArgs.push_back(output.c_str());
        }
        break;
        case Project::Type::ZLIB:
        {
            break;
        }
        break;
        case Project::Type::STATIC_LIB:
        {
            lldArgs.push_back("llvm-ar");
            lldArgs.push_back("rcs");
            lldArgs.push_back(std::format("{}.a", output).c_str());
        }
        break;
        case Project::Type::SHARED_LIB:
        {
            lldArgs.push_back("ld64.lld");
            lldArgs.push_back("-o");
            lldArgs.push_back(std::format("{}.dylib", output).c_str());
        }
        break;
        }
        if (project.debugBuild)
        {
            lldArgs.push_back("-g");
        }
        if (!project.linkerFlags.entry.empty())
        {
            lldArgs.push_back("-e");
            lldArgs.push_back(project.linkerFlags.entry.c_str());
        }
        if (!project.linkerFlags.noCRT)
        {
            switch (project.linkerFlags.crtLinkType)
            {
            case Project::LinkerFlags::CRT_LinkType::STATIC:
            {
                lldArgs.push_back("/use/lib/libSystem.B.tbd");
            }
            break;
            case Project::LinkerFlags::CRT_LinkType::DYNAMIC:
            {
                lldArgs.push_back("-lSystem");
                lldArgs.push_back("-syslibroot $(xcrun --show-sdk-path)");
            }
            break;
            }
        }
    }
    else if (project.platform == Project::Platform::LINUX)
    {
        for (auto &nl : project.linkerFlags.nativeLibs)
        {
            nl.relative_path = nl.relative_path.generic_string() + ".a";
        }
        switch (project.type)
        {
        case Project::Type::EXE:
        {
            lldArgs.push_back("ld.lld");
            lldArgs.push_back(objFileName.c_str());
            lldArgs.push_back("-o");
        }
        break;
        case Project::Type::ZLIB:
        {
            break;
        }
        break;
        case Project::Type::STATIC_LIB:
        {
            lldArgs.push_back("llvm-ar");
            lldArgs.push_back("rcs");
            lldArgs.push_back(std::format("{}.a", output).c_str());
        }
        break;
        case Project::Type::SHARED_LIB:
        {
            lldArgs.push_back("ld.lld");
            lldArgs.push_back("-shared");
            lldArgs.push_back("-o");
            lldArgs.push_back(std::format("{}.so", output).c_str());
        }
        break;
        }
        if (project.debugBuild)
        {
            lldArgs.push_back("-g");
        }
        if (!project.linkerFlags.entry.empty())
        {
            lldArgs.push_back("-e");
            lldArgs.push_back(project.linkerFlags.entry.c_str());
        }
        if (!project.linkerFlags.noCRT)
        {
            switch (project.linkerFlags.crtLinkType)
            {
            case Project::LinkerFlags::CRT_LinkType::STATIC:
            {
                lldArgs.push_back("/usr/lib/crt1.o");
                lldArgs.push_back("/usr/lib/crti.o");
                lldArgs.push_back("-lc");
                lldArgs.push_back("/usr/lib/crtn.o");
            }
            break;
            case Project::LinkerFlags::CRT_LinkType::DYNAMIC:
            {
                lldArgs.push_back("/usr/lib/crt1.o");
                lldArgs.push_back("/usr/lib/crti.o");
                lldArgs.push_back("-libc.so");
                lldArgs.push_back("/usr/lib/crtn.o");
            }
            break;
            }
        }
    }
    else
    {
        PrintError("Unsupported target platform");
    }

    std::vector<const char *> lldArgs_cstr;
    for (auto &arg : lldArgs)
    {
        lldArgs_cstr.push_back(arg.c_str());
    }
    printf("Building %s...\n", project.name.c_str());
    printf("Linker Flags: \n");
    for (auto f : lldArgs_cstr)
    {
        printf("%s\n", f);
    }
    printf("\n\n");
    lld::Result result =
        lld::lldMain(lldArgs_cstr, llvm::outs(), llvm::errs(), LLD_ALL_DRIVERS);
    if (result.retCode)
    {
        PrintError("Failed to link obj files.");
    }
}

llvm::Constant *
CodeGen::InitListToArrayConstant(ArrayDataType *arrType,
                                 InitializerListExpression *listExpr)
{
    std::vector<llvm::Constant *> consts;
    for (Expression *expr : listExpr->GetExpressions())
    {
        auto le = dynamic_cast<InitializerListExpression *>(expr);
        IRValue value;
        if (le && arrType->dataType->GetType() == DataType::Type::ARRAY)
        {
            auto type = dynamic_cast<ArrayDataType *>(arrType->dataType);
            value.value = InitListToArrayConstant(type, le);
            value.type = ZtoonTypeToLLVMType(type);
        }
        else if (le && arrType->dataType->GetType() == DataType::Type::STRUCT)
        {
            auto type = dynamic_cast<StructDataType *>(arrType->dataType);
            value.value = InitListToStructConstant(type, le);
            value.type = ZtoonTypeToLLVMType(type);
        }
        else
        {
            value = GenExpressionIR(expr);
        }
        llvm::Constant *valueConst =
            llvm::dyn_cast<llvm::Constant>(value.value);
        if (!valueConst)
        {
            ReportError("Expression is not known at compile time",
                        expr->GetCodeErrString());
        }
        consts.push_back(valueConst);
    }

    return llvm::ConstantArray::get(
        llvm::dyn_cast<llvm::ArrayType>(ZtoonTypeToLLVMType(arrType).type),
        consts);
}
llvm::Constant *
CodeGen::InitListToStructConstant(StructDataType *structType,
                                  InitializerListExpression *listExpr)
{
    std::vector<llvm::Constant *> consts;
    if (structType->fields.size() != listExpr->GetExpressions().size())
    {
        ReportError(
            std::format("List expression does not match struct '{}' size",
                        structType->GetName()),
            listExpr->GetCodeErrString());
    }
    for (size_t i = 0; i < structType->fields.size(); i++)
    {
        auto expr = listExpr->GetExpressions()[i];
        auto fieldType = structType->fields[i];
        auto le = dynamic_cast<InitializerListExpression *>(expr);
        IRValue value;
        if (expr && le && fieldType->GetType() == DataType::Type::ARRAY)
        {
            auto type = dynamic_cast<ArrayDataType *>(fieldType);
            value.value = InitListToArrayConstant(type, le);
            value.type = ZtoonTypeToLLVMType(type);
        }
        else if (expr && le && fieldType->GetType() == DataType::Type::STRUCT)
        {
            auto type = dynamic_cast<StructDataType *>(fieldType);
            value.value = InitListToStructConstant(type, le);
            value.type = ZtoonTypeToLLVMType(type);
        }
        else
        {
            if (expr)
            {

                llvm::BasicBlock *block = irBuilder->GetInsertBlock();
                auto pe = dynamic_cast<PrimaryExpression *>(expr);
                if (!block && pe &&
                    pe->GetPrimary()->GetType() == TokenType::IDENTIFIER)
                {
                    semanticAnalyzer.currentScope->importedPackages.insert(
                        semanticAnalyzer.currentScope->importedPackages.begin(),
                        structType->scope);
                    auto var = dynamic_cast<Variable *>(
                        semanticAnalyzer.currentScope->GetSymbol(
                            pe->GetPrimary()->GetLexeme(),
                            pe->GetCodeErrString()));
                    auto irSymbol = GetIRSymbol(pe->GetPrimary()->GetLexeme());
                    if (!irSymbol)
                    {
                        GenGlobalVariableIR(var->varDeclStmt);
                    }
                    semanticAnalyzer.currentScope->importedPackages.erase(
                        semanticAnalyzer.currentScope->importedPackages
                            .begin());
                    if (var && globalConstsMap.contains(var->varDeclStmt))
                    {
                        value = globalConstsMap[var->varDeclStmt];
                    }
                    else
                    {
                        ReportError("Expression datatype is not readonly",
                                    expr->GetCodeErrString());
                    }
                }
                else
                {
                    value = GenExpressionIR(expr);
                    if (value.value->getType()->isPointerTy())
                    {
                        value.value =
                            irBuilder->CreateLoad(value.type.type, value.value);
                    }
                }
            }
            else
            {
                value.value = llvm::Constant::getNullValue(
                    ZtoonTypeToLLVMType(fieldType).type);
            }
        }

        llvm::Constant *valueConst =
            llvm::dyn_cast<llvm::Constant>(value.value);
        if (!valueConst)
        {
            ReportError("Expression is not known at compile time",
                        expr->GetCodeErrString());
        }
        consts.push_back(valueConst);
    }
    llvm::StructType *st =
        llvm::dyn_cast<llvm::StructType>(ZtoonTypeToLLVMType(structType).type);
    size_t stSize = st->elements().size();
    size_t c = consts.size();
    bool eq = stSize == c;
    if (eq)
    {
        for (size_t i = 0; i < c; i++)
        {
            auto sF = st->elements()[i];
            auto lF = consts[i]->getType();
            assert(sF == lF);
        }
    }
    else
    {
        assert(0);
    }
    return llvm::ConstantStruct::get(st, consts);
}
void CodeGen::AssignValueToVarArray(IRValue ptr, Expression *expr,
                                    ArrayDataType *arrType,
                                    std::vector<llvm::Value *> &index)

{
    auto initListExpr = dynamic_cast<InitializerListExpression *>(expr);
    auto rValueArrType =
        dynamic_cast<ArrayDataType *>(semanticAnalyzer.exprToDataTypeMap[expr]);
    if (initListExpr)
    {
        if (initListExpr->GetExpressions().empty())
        {
            llvm::Value *GEP = nullptr;
            if (ptr.type.type->isArrayTy())
            {
                GEP = irBuilder->CreateInBoundsGEP(
                    ptr.type.type, ptr.value,
                    {irBuilder->getInt32(0), irBuilder->getInt32(0)});
            }
            else
            {
                GEP = ptr.value;
            }
            // copy data
            IRType elementType = ZtoonTypeToLLVMType(arrType->dataType);
            size_t arraySizeInBytes =
                arrType->size *
                (moduleDataLayout->getTypeAllocSize(elementType.type));
            llvm::Value *sizeValue = irBuilder->getInt64(arraySizeInBytes);
            llvm::Align alignment(
                moduleDataLayout->getTypeAllocSize(elementType.type));
            llvm::Value *zeroValue = llvm::Constant::getNullValue(
                ZtoonTypeToLLVMType(arrType->dataType).type);
            GEP = irBuilder->CreatePointerCast(
                GEP, llvm::Type::getInt8Ty(*ctx)->getPointerTo());
            irBuilder->CreateMemSet(
                GEP, llvm::ConstantInt::get(llvm::Type::getInt8Ty(*ctx), 0),
                sizeValue, alignment);
            return;
        }
        else if (arrType->size != initListExpr->GetExpressions().size())
        {
            ReportError("Initializer list size does not match array size",
                        initListExpr->GetCodeErrString());
        }

        auto innerType = dynamic_cast<ArrayDataType *>(arrType->dataType);
        IRType arrIRType = ZtoonTypeToLLVMType(arrType);
        if (ptr.type.type->isArrayTy() && index.size() == 0)
        {
            index.push_back(irBuilder->getInt32(0));
        }
        index.push_back(irBuilder->getInt32(0));
        size_t i = 0;
        for (auto elementExpr : initListExpr->GetExpressions())
        {
            index.back() = irBuilder->getInt32(i);
            i++;
            if (innerType)
            {
                AssignValueToVarArray(ptr, elementExpr, innerType, index);
            }
            else
            {
                IRValue value = GenExpressionIR(elementExpr);

                llvm::Value *GEP = irBuilder->CreateInBoundsGEP(
                    ptr.type.type, ptr.value, index,
                    std::format("element_{}", i));
                if (dynamic_cast<StructDataType *>(arrType->dataType) &&
                    elementExpr)
                {
                    auto structType =
                        dynamic_cast<StructDataType *>(arrType->dataType);
                    AssignValueToVarStruct({GEP, {}}, elementExpr, structType);
                }
                else
                {
                    irBuilder->CreateStore(value.value, GEP);
                }
            }
        }
        index.pop_back();
    }
    else if (rValueArrType)
    {

        std::string r = rValueArrType->ToString();
        std::string l = arrType->ToString();
        removeReadonlyPrefix(r);
        removeReadonlyPrefix(l);
        if (r != l)
        {
            ReportError(std::format("Type {} and type {} are not compatible",
                                    arrType->ToString(),
                                    rValueArrType->ToString()),
                        expr->GetCodeErrString());
        }
        // copy data
        IRType elementType = ZtoonTypeToLLVMType(rValueArrType->dataType);
        size_t arraySizeInBytes =
            rValueArrType->size *
            (moduleDataLayout->getTypeSizeInBits(elementType.type) / 8);
        llvm::Value *sizeValue = irBuilder->getInt64(arraySizeInBytes);
        llvm::Align alignment(
            moduleDataLayout->getTypeSizeInBits(elementType.type) / 8);
        IRValue src = GenExpressionIR(expr);
        if (src.type.type->isArrayTy())
        {
            llvm::Value *arrAlloca =
                irBuilder->CreateAlloca(ZtoonTypeToLLVMType(rValueArrType).type,
                                        nullptr, expr->GetCodeErrString().str);
            irBuilder->CreateStore(src.value, arrAlloca);
            src.value = irBuilder->CreateInBoundsGEP(
                ZtoonTypeToLLVMType(rValueArrType).type, arrAlloca,
                {irBuilder->getInt32(0), irBuilder->getInt32(0)});
            src.type.type = ptr.value->getType();
        }
        llvm::Value *dest =
            irBuilder->CreateInBoundsGEP(ptr.type.type, ptr.value, index);
        irBuilder->CreateMemCpy(dest, alignment, src.value, alignment,
                                sizeValue);
    }
    else
    {
        // error
        ReportError(
            std::format(
                "Cannot assign expression of type '{}' to array type '{}'",
                semanticAnalyzer.exprToDataTypeMap[expr]->ToString(),
                arrType->ToString()),
            expr->GetCodeErrString());
    }
}

void CodeGen::AssignValueToVarStruct(IRValue ptr, Expression *expr,
                                     StructDataType *structType)
{
    auto exprListType =
        dynamic_cast<InitListType *>(semanticAnalyzer.exprToDataTypeMap[expr]);
    auto exprStructType = dynamic_cast<StructDataType *>(
        semanticAnalyzer.exprToDataTypeMap[expr]);
    IRType irStructType = ZtoonTypeToLLVMType(structType);
    if (exprListType)
    {
        auto listExpr = dynamic_cast<InitializerListExpression *>(expr);
        size_t listSize = listExpr->GetExpressions().size();
        size_t structSize = structType->fields.size();
        if (listSize != structSize)
        {
            ReportError(
                std::format("List expression does not match struct '{}' size",
                            structType->GetName()),
                listExpr->GetCodeErrString());
        }

        for (size_t index = 0; index < listExpr->GetExpressions().size();
             index++)
        {

            llvm::Value *fieldPtr =
                irBuilder->CreateStructGEP(irStructType.type, ptr.value, index);
            auto listElement = listExpr->GetExpressions()[index];
            auto fieldType = structType->fields[index];

            if (fieldType->GetType() == DataType::Type::STRUCT && listElement)
            {
                AssignValueToVarStruct(
                    {fieldPtr, {false, fieldPtr->getType()}}, listElement,
                    dynamic_cast<StructDataType *>(fieldType));
            }
            else if (fieldType->GetType() == DataType::Type::ARRAY &&
                     listElement)
            {
                std::vector<llvm::Value *> indices;
                AssignValueToVarArray(
                    {fieldPtr, {false, fieldPtr->getType()}}, listElement,
                    dynamic_cast<ArrayDataType *>(fieldType), indices);
            }
            else
            {
                IRValue fieldValue;
                if (listElement)
                {
                    fieldValue = GenExpressionIR(listElement);
                }
                else
                {
                    fieldValue.value = llvm::Constant::getNullValue(
                        ZtoonTypeToLLVMType(fieldType).type);
                }

                irBuilder->CreateStore(fieldValue.value, fieldPtr);
            }
        }
    }
    else if (exprStructType)
    {

        if (exprStructType->ToString() != structType->ToString())
        {
            ReportError(std::format("Type {} and type {} are not compatible",
                                    structType->ToString(),
                                    exprStructType->ToString()),
                        expr->GetCodeErrString());
        }
        // copy data
        size_t structSizeInBytes =
            moduleDataLayout->getTypeAllocSize(irStructType.type);
        llvm::Value *sizeValue = irBuilder->getInt64(structSizeInBytes);
        llvm::Align alignment(
            moduleDataLayout->getABITypeAlign(irStructType.type));
        IRValue src = GenExpressionIR(expr);

        IRValue dest = ptr;
        if (src.value->getType()->isStructTy())
        {
            irBuilder->CreateStore(src.value, dest.value);
        }
        else
        {
            irBuilder->CreateMemCpy(dest.value, alignment, src.value, alignment,
                                    sizeValue);
        }
    }
    else
    {
        // error
        ReportError(
            std::format(
                "Cannot assign expression of type '{}' to struct type '{}'",
                semanticAnalyzer.exprToDataTypeMap[expr]->ToString(),
                structType->ToString()),
            expr->GetCodeErrString());
    }
}
CodeGen::~CodeGen() {}

void CodeGen::GenIR(std::vector<Package *> &packages, bool genTypeSymbol,
                    bool genFnAndVarSymbol, bool genTypeBody)
{
    if (genTypeSymbol)
    {
        for (auto pkg : packages)
        {
            GenPackageGlobalTypesIR(pkg);
        }
    }
    if (genFnAndVarSymbol)
    {
        for (auto pkg : packages)
        {
            GenPackageGlobalFuncsAndVarsIR(pkg);
        }
    }
    if (genTypeBody)
    {
        for (auto pkg : packages)
        {
            GenPackageGlobalVarAndFuncBodiesIR(pkg);
        }
    }
}

void CodeGen::GenPackageGlobalTypesIR(Package *pkg)
{
    semanticAnalyzer.currentPackage = pkg;
    semanticAnalyzer.currentScope = semanticAnalyzer.pkgToScopeMap[pkg];

    std::function<void(StructDataType *)> genStructIR = nullptr;
    std::function<void(UnionDataType *)> genUnionIR = nullptr;

    genStructIR = [&](StructDataType *structZtoonType)
    {
        llvm::StructType *structIRType =
            llvm::StructType::getTypeByName(*ctx, structZtoonType->GetUID());
        if (!structIRType)
        {
            structIRType =
                llvm::StructType::create(*ctx, structZtoonType->GetUID());
            std::vector<llvm::Type *> bodyFields;
            for (DataType *fieldType : structZtoonType->fields)
            {

                if (fieldType->GetType() == DataType::Type::STRUCT)
                {
                    auto fieldStructType =
                        dynamic_cast<StructDataType *>(fieldType);

                    llvm::StructType *structFieldIRType =
                        llvm::StructType::getTypeByName(
                            *ctx, fieldStructType->GetUID());
                    if (!structFieldIRType)
                    {
                        genStructIR(fieldStructType);
                    }
                    structFieldIRType = llvm::StructType::getTypeByName(
                        *ctx, fieldStructType->GetUID());
                    bodyFields.push_back(structFieldIRType);
                }
                else if (fieldType->GetType() == DataType::Type::UNION)
                {
                    auto fieldUnionType =
                        dynamic_cast<UnionDataType *>(fieldType);
                    if (!fieldUnionType->largestDatatype)
                    {
                        genUnionIR(fieldUnionType);
                    }
                    bodyFields.push_back(
                        ZtoonTypeToLLVMType(fieldUnionType->largestDatatype)
                            .type);
                }
                else
                {
                    IRType irFieldType = ZtoonTypeToLLVMType(fieldType);
                    bodyFields.push_back(irFieldType.type);
                }
            }
            structIRType->setBody(bodyFields);
        }
    };

    genUnionIR = [&](UnionDataType *unionZtoonType)
    {
        if (!unionZtoonType->largestDatatype)
        {
            // Get Largest datatype.
            size_t largestSize = 0;
            for (DataType *fieldType : unionZtoonType->fields)
            {

                if (fieldType->GetType() == DataType::Type::STRUCT)
                {
                    auto fieldStructType =
                        dynamic_cast<StructDataType *>(fieldType);

                    llvm::StructType *structFieldIRType =
                        llvm::StructType::getTypeByName(
                            *ctx, fieldStructType->GetName());

                    if (!structFieldIRType)
                    {
                        // need to do soemthing
                        genStructIR(fieldStructType);
                    }
                }
                else if (fieldType->GetType() == DataType::Type::UNION)
                {
                    auto fieldUnionType =
                        dynamic_cast<UnionDataType *>(fieldType);
                    if (!fieldUnionType->largestDatatype)
                    {
                        genUnionIR(fieldUnionType);
                    }
                }

                IRType irFieldType = ZtoonTypeToLLVMType(fieldType);
                size_t sizeInBytes =
                    moduleDataLayout->getTypeSizeInBits(irFieldType.type) / 8;
                if (largestSize < sizeInBytes)
                {
                    unionZtoonType->largestDatatype = fieldType;
                    largestSize = sizeInBytes;
                }
            }
        }
    };

    for (auto stmt : pkg->GetStatements())
    {
        if (dynamic_cast<StructStatement *>(stmt))
        {
            StructStatement *structStmt = dynamic_cast<StructStatement *>(stmt);
            if (structStmt->generic)
            {
                continue;
            }
            auto structZtoonType = dynamic_cast<StructDataType *>(
                semanticAnalyzer.stmtToDataTypeMap[structStmt]);
            genStructIR(structZtoonType);
            auto temp = semanticAnalyzer.currentScope;
            semanticAnalyzer.currentScope = structZtoonType->scope;
            for (auto method : structStmt->GetMethods())
            {
                if (method->GetGeneric())
                {
                    continue;
                }
                FnStatement *fnStmt = method;
                GenFnStatementIR(fnStmt, true, false);
            }
            semanticAnalyzer.currentScope = temp;
        }
        else if (dynamic_cast<UnionStatement *>(stmt))
        {
            UnionStatement *unionStmt = dynamic_cast<UnionStatement *>(stmt);
            auto unionType = dynamic_cast<UnionDataType *>(
                semanticAnalyzer.stmtToDataTypeMap[unionStmt]);
            genUnionIR(unionType);
        }
        else if (dynamic_cast<EnumStatement *>(stmt))
        {
            GenStatementIR(stmt);
        }
    }
}
void CodeGen::GenGlobalVariableIR(VarDeclStatement *varDeclStatement)
{

    globalStatementIRDoneMap[varDeclStatement] = true;
    UnionDataType *unionType = dynamic_cast<UnionDataType *>(
        semanticAnalyzer.stmtToDataTypeMap[varDeclStatement]);
    if (unionType)
    {
        semanticAnalyzer.stmtToDataTypeMap[varDeclStatement] =
            unionType->largestDatatype;
    }
    DataType *type = semanticAnalyzer.stmtToDataTypeMap[varDeclStatement];
    ArrayDataType *arrType = dynamic_cast<ArrayDataType *>(type);
    StructDataType *structType = dynamic_cast<StructDataType *>(type);

    IRType varDeclType = {};
    varDeclType.type = ZtoonTypeToLLVMType(type).type;
    varDeclType.isSigned = type->IsSigned();

    std::string varName =
        std::format("{}::{}", semanticAnalyzer.currentScope->name,
                    varDeclStatement->GetIdentifier()->GetLexeme());
    Variable *var =
        dynamic_cast<Variable *>(semanticAnalyzer.currentScope->GetSymbol(
            varDeclStatement->GetIdentifier()->GetLexeme(),
            varDeclStatement->GetCodeErrString()));

    llvm::GlobalVariable *globalVar;
    if (varDeclStatement->GetExpression())
    {
        auto pe = dynamic_cast<PrimaryExpression *>(
            varDeclStatement->GetExpression());

        if (pe && pe->GetPrimary()->GetType() == TokenType::IDENTIFIER)
        {
            auto irSymbol = GetIRSymbol(pe->GetPrimary()->GetLexeme());
            if (!irSymbol)
            {
                Variable *rVar = dynamic_cast<Variable *>(
                    semanticAnalyzer.currentScope->GetSymbol(
                        pe->GetPrimary()->GetLexeme(),
                        varDeclStatement->GetExpression()->GetCodeErrString()));

                GenGlobalVariableIR(rVar->varDeclStmt);
            }
        }

        if (arrType || structType)
        {
            auto listExpr = dynamic_cast<InitializerListExpression *>(
                varDeclStatement->GetExpression());
            if (!listExpr)
            {
                ReportError(
                    std::format("Global {} variables can only be initialized "
                                "with a list of compile time expressions",
                                arrType ? " array " : "struct"),
                    varDeclStatement->GetCodeErrString());
            }
            llvm::Constant *consts =
                arrType ? InitListToArrayConstant(arrType, listExpr)
                        : InitListToStructConstant(structType, listExpr);

            globalVar = new llvm::GlobalVariable(
                *module, varDeclType.type, type->IsReadOnly(),
                llvm::GlobalValue::ExternalLinkage, consts, varName);
            globalConstsMap[varDeclStatement] = {consts, false};
        }
        else
        {

            bool isExprGlobalVar =
                pe ? pe->GetPrimary()->GetType() == TokenType::IDENTIFIER
                   : false;
            IRValue exprValue =
                GenExpressionIR(varDeclStatement->GetExpression());
            if (!llvm::isa<llvm::Constant>(exprValue.value) ||
                type->GetType() == DataType::Type::FN)
            {
                ReportError(
                    std::format("Expression '{}' is not known at "
                                "compile time",
                                varDeclStatement->GetExpression()
                                    ->GetCodeErrString()
                                    .str),
                    varDeclStatement->GetExpression()->GetCodeErrString());
            }
            llvm::Constant *constValue =
                llvm::cast<llvm::Constant>(exprValue.value);
            globalVar = new llvm::GlobalVariable(
                *module, varDeclType.type, type->IsReadOnly(),
                llvm::GlobalValue::ExternalLinkage,
                isExprGlobalVar
                    ? llvm::cast<llvm::GlobalVariable>(exprValue.value)
                          ->getInitializer()
                    : constValue,
                varName);
            globalConstsMap[varDeclStatement] = {constValue, false};
        }
    }
    else
    {
        llvm::Constant *constValue =
            structType ? InitListToStructConstant(structType,
                                                  structType->defaultValuesList)
                       : llvm::Constant::getNullValue(varDeclType.type);
        globalVar = new llvm::GlobalVariable(
            *module, varDeclType.type, type->IsReadOnly(),
            llvm::GlobalValue::ExternalLinkage, constValue, varName);

        globalConstsMap[varDeclStatement] = {constValue, false};
    }

    IRVariable *irVariable = gZtoonArena.Allocate<IRVariable>();
    irVariable->value = globalVar;
    irVariable->variabel =
        dynamic_cast<Variable *>(semanticAnalyzer.currentScope->GetSymbol(
            varDeclStatement->GetIdentifier()->GetLexeme(),
            varDeclStatement->GetCodeErrString()));
    irVariable->irType = varDeclType;
    AddIRSymbol(irVariable);
}
void CodeGen::GenPackageGlobalFuncsAndVarsIR(Package *pkg)
{
    semanticAnalyzer.currentPackage = pkg;
    semanticAnalyzer.currentScope = semanticAnalyzer.pkgToScopeMap[pkg];

    for (auto stmt : pkg->GetStatements())
    {
        if (dynamic_cast<VarDeclStatement *>(stmt))
        {
            VarDeclStatement *varDeclStatement =
                dynamic_cast<VarDeclStatement *>(stmt);

            if (!globalStatementIRDoneMap.contains(varDeclStatement))
                GenGlobalVariableIR(varDeclStatement);
        }
        else if (dynamic_cast<FnStatement *>(stmt))
        {
            FnStatement *fnStmt = dynamic_cast<FnStatement *>(stmt);

            Function *fn = dynamic_cast<Function *>(
                semanticAnalyzer.currentScope->GetSymbol(
                    fnStmt->GetIdentifier()->GetLexeme(),
                    fnStmt->GetCodeErrString()));
            if (fn->generic)
            {
                continue;
            }
            std::string fpName = fnStmt->GetIdentifier()->GetLexeme();

            if (!fnStmt->IsPrototype())
            {
                if (fpName != "main")
                {
                    fpName =
                        semanticAnalyzer.currentScope->name + "::" + fpName;
                }
            }
            else
            {
                if (llvm::Function *func = module->getFunction(fpName))
                {
                    IRFunction *irFunc = gZtoonArena.Allocate<IRFunction>();
                    irFunc->fn = func;
                    irFunc->fnType = func->getFunctionType();
                    irFunc->ztoonFn = fn;
                    irFunc->name = fpName;
                    irFunc->fullName = fpName;
                    symbolToIRSymobMap[fn] = irFunc;
                    continue;
                }
            }
            GenFnStatementIR(fnStmt, true, false);
        }
    }
}

void CodeGen::GenPackageGlobalVarAndFuncBodiesIR(Package *pkg)
{
    semanticAnalyzer.currentPackage = pkg;
    semanticAnalyzer.currentScope = semanticAnalyzer.pkgToScopeMap[pkg];
    for (auto stmt : pkg->GetStatements())
    {
        if (dynamic_cast<FnStatement *>(stmt))
        {
            FnStatement *fnStmt = dynamic_cast<FnStatement *>(stmt);
            Function *fn = dynamic_cast<Function *>(
                semanticAnalyzer.currentScope->GetSymbol(
                    fnStmt->GetIdentifier()->GetLexeme(),
                    fnStmt->GetCodeErrString()));

            if (fn->generic)
            {
                continue;
            }
            FnDataType *fnDataType = fn->GetFnDataTypeFromFnPTR();

            IRFunction *irFunc = dynamic_cast<IRFunction *>(
                GetIRSymbol(fnStmt->GetIdentifier()->GetLexeme()));
            auto tempBlockStmt = semanticAnalyzer.currentBlockStatement;
            semanticAnalyzer.currentBlockStatement =
                fnStmt->GetBlockStatement();
            if (!fnStmt->IsPrototype())
            {
                llvm::BasicBlock *fnBB = llvm::BasicBlock::Create(
                    *ctx, std::format("{}FnBlock", fn->GetName()), irFunc->fn);
                irFunc->fnBB = fnBB;
                irBuilder->SetInsertPoint(fnBB);

                size_t index = 0;
                auto tempScope = semanticAnalyzer.currentScope;
                semanticAnalyzer.currentScope =
                    semanticAnalyzer
                        .blockToScopeMap[fnStmt->GetBlockStatement()];
                for (auto aggType : semanticAnalyzer.fnToAggDeclsMap[fn])
                {
                    auto temp = semanticAnalyzer.currentScope;
                    if (aggType.structStmt)
                    {
                        GenStructStatementIR(aggType.structStmt, true, false);
                    }
                }
                for (VarDeclStatement *paramStmt : fnStmt->GetParameters())
                {
                    GenVarDeclStatementIR(paramStmt, true, false);
                }

                for (auto varDecl : semanticAnalyzer.fnToVarDeclsMap[fn])
                {
                    auto temp = semanticAnalyzer.currentScope;

                    semanticAnalyzer.currentScope = varDecl.currentScope;
                    GenVarDeclStatementIR(varDecl.varDecl, true, false);
                    semanticAnalyzer.currentScope = temp;
                }

                for (VarDeclStatement *paramStmt : fnStmt->GetParameters())
                {
                    GenVarDeclStatementIR(paramStmt, false, true);
                    IRVariable *paramVar = dynamic_cast<IRVariable *>(
                        GetIRSymbol(paramStmt->GetIdentifier()->GetLexeme()));
                    llvm::Value *value = irFunc->fn->getArg(index);
                    irBuilder->CreateStore(value, paramVar->value);
                    index++;
                }

                GenStatementIR(fnStmt->GetBlockStatement());

                llvm::Instruction *term =
                    irBuilder->GetInsertBlock()->getTerminator();

                if (!term || !llvm::isa<llvm::ReturnInst>(term))
                {
                    if (irFunc->ztoonFn->GetFnDataTypeFromFnPTR()
                            ->returnDataType->type != DataType::Type::NOTYPE)
                    {
                        irBuilder->CreateRet(
                            GenExpressionIR(
                                irFunc->ztoonFn->retStmt->GetExpression())
                                .value);
                    }
                    else
                    {
                        irBuilder->CreateRetVoid();
                    }
                }

                semanticAnalyzer.currentScope = tempScope;
            }
            semanticAnalyzer.currentBlockStatement = tempBlockStmt;
        }
        else if (dynamic_cast<StructStatement *>(stmt))
        {
            StructStatement *structStmt = dynamic_cast<StructStatement *>(stmt);
            if (structStmt->generic)
            {
                continue;
            }
            auto structZtoonType = dynamic_cast<StructDataType *>(
                semanticAnalyzer.stmtToDataTypeMap[structStmt]);
            auto tempScope = semanticAnalyzer.currentScope;
            semanticAnalyzer.currentScope = structZtoonType->scope;
            for (auto method : structStmt->GetMethods())
            {

                if (method->GetGeneric())
                {
                    continue;
                }
                FnStatement *fnStmt = method;
                Function *fn = dynamic_cast<Function *>(
                    semanticAnalyzer.currentScope->GetSymbol(
                        fnStmt->GetIdentifier()->GetLexeme(),
                        fnStmt->GetCodeErrString()));

                FnDataType *fnDataType = fn->GetFnDataTypeFromFnPTR();

                IRFunction *irFunc = dynamic_cast<IRFunction *>(
                    GetIRSymbol(fnStmt->GetIdentifier()->GetLexeme()));

                llvm::BasicBlock *fnBB = llvm::BasicBlock::Create(
                    *ctx, std::format("{}FnBlock", fn->GetName()), irFunc->fn);
                irFunc->fnBB = fnBB;
                irBuilder->SetInsertPoint(fnBB);

                size_t index = 0;
                auto tempScope = semanticAnalyzer.currentScope;
                semanticAnalyzer.currentScope =
                    semanticAnalyzer
                        .blockToScopeMap[fnStmt->GetBlockStatement()];
                for (VarDeclStatement *paramStmt : fnStmt->GetParameters())
                {
                    GenVarDeclStatementIR(paramStmt, true, false);
                }

                for (auto varDecl : semanticAnalyzer.fnToVarDeclsMap[fn])
                {
                    auto temp = semanticAnalyzer.currentScope;
                    semanticAnalyzer.currentScope = varDecl.currentScope;
                    GenVarDeclStatementIR(varDecl.varDecl, true, false);
                    semanticAnalyzer.currentScope = temp;
                }

                for (VarDeclStatement *paramStmt : fnStmt->GetParameters())
                {
                    GenVarDeclStatementIR(paramStmt, false, true);
                    IRVariable *paramVar = dynamic_cast<IRVariable *>(
                        GetIRSymbol(paramStmt->GetIdentifier()->GetLexeme()));
                    llvm::Value *value = irFunc->fn->getArg(index);
                    irBuilder->CreateStore(value, paramVar->value);
                    index++;
                }

                GenStatementIR(fnStmt->GetBlockStatement());

                llvm::Instruction *term =
                    irBuilder->GetInsertBlock()->getTerminator();

                if (!term || !llvm::isa<llvm::ReturnInst>(term))
                {
                    if (irFunc->ztoonFn->GetFnDataTypeFromFnPTR()
                            ->returnDataType->type != DataType::Type::NOTYPE)
                    {
                        irBuilder->CreateRet(
                            GenExpressionIR(
                                irFunc->ztoonFn->retStmt->GetExpression())
                                .value);
                    }
                    else
                    {
                        irBuilder->CreateRetVoid();
                    }
                }

                semanticAnalyzer.currentScope = tempScope;
            }
            semanticAnalyzer.currentScope = tempScope;
        }
    }
}

void CodeGen::GenStatementIR(Statement *statement)
{
    if (dynamic_cast<StructStatement *>(statement))
    {
        auto structStmt = dynamic_cast<StructStatement *>(statement);
        GenStructStatementIR(structStmt, false, true);
    }
    else if (dynamic_cast<EnumStatement *>(statement))
    {
        auto enumStmt = dynamic_cast<EnumStatement *>(statement);
        GenEnumStatementIR(enumStmt, true, true);
    }
    else if (dynamic_cast<UnionStatement *>(statement))
    {
        auto unionStmt = dynamic_cast<UnionStatement *>(statement);
        GenUnionStatementIR(unionStmt, true, true);
    }
    else if (dynamic_cast<VarDeclStatement *>(statement))
    {
        auto varDeclStmt = dynamic_cast<VarDeclStatement *>(statement);
        GenVarDeclStatementIR(varDeclStmt, false, true);
    }
    else if (dynamic_cast<VarAssignmentStatement *>(statement))
    {
        // Need to check if ptr or variable.
        VarAssignmentStatement *varAssignmentStatement =
            dynamic_cast<VarAssignmentStatement *>(statement);
        GenVarAssignmentStatementIR(varAssignmentStatement);
    }
    else if (dynamic_cast<VarCompoundAssignmentStatement *>(statement))
    {
        VarCompoundAssignmentStatement *varComAssignStatement =
            dynamic_cast<VarCompoundAssignmentStatement *>(statement);

        // varComAssignStatement->GetLValue()->GetFirstToken());
        GenVarCompundAssignmentStatementIR(varComAssignStatement);
    }
    else if (dynamic_cast<ExpressionStatement *>(statement))
    {
        ExpressionStatement *exprStatement =
            dynamic_cast<ExpressionStatement *>(statement);
        // exprStatement->GetExpression()->GetFirstToken());
        GenExpressionStatementIR(exprStatement);
    }
    else if (dynamic_cast<BlockStatement *>(statement))
    {
        BlockStatement *blockStatement =
            dynamic_cast<BlockStatement *>(statement);
        GenBlockStatementIR(blockStatement);
    }
    else if (dynamic_cast<IfStatement *>(statement))
    {
        IfStatement *ifStatement = dynamic_cast<IfStatement *>(statement);
        GenIfStatementIR(ifStatement);
    }
    else if (dynamic_cast<WhileLoopStatement *>(statement))
    {
        WhileLoopStatement *whileStatement =
            dynamic_cast<WhileLoopStatement *>(statement);
        GenWhileLoopStatementIR(whileStatement);
    }
    else if (dynamic_cast<ForLoopStatement *>(statement))
    {
        ForLoopStatement *forLoopStatement =
            dynamic_cast<ForLoopStatement *>(statement);
        GenForLoopStatementIR(forLoopStatement);
    }
    else if (dynamic_cast<BreakStatement *>(statement))
    {
        auto bStmt = dynamic_cast<BreakStatement *>(statement);
        GenBreakStatementIR(bStmt);
    }
    else if (dynamic_cast<ContinueStatement *>(statement))
    {
        auto cStmt = dynamic_cast<ContinueStatement *>(statement);
        GenContinueStatementIR(cStmt);
    }
    else if (dynamic_cast<RetStatement *>(statement))
    {
        auto *retStmt = dynamic_cast<RetStatement *>(statement);
        GenRetStatementIR(retStmt);
    }
}

void CodeGen::GenBlockStatementIR(BlockStatement *blockStmt)
{
    Scope *temp = semanticAnalyzer.currentScope;
    BlockStatement *tempBlock = semanticAnalyzer.currentBlockStatement;
    semanticAnalyzer.currentBlockStatement = blockStmt;
    semanticAnalyzer.currentScope = semanticAnalyzer.blockToScopeMap[blockStmt];
    for (Statement *s : blockStmt->GetStatements())
    {
        GenStatementIR(s);
    }

    semanticAnalyzer.currentScope = temp;
    semanticAnalyzer.currentBlockStatement = tempBlock;
}
void CodeGen::GenFnStatementIR(FnStatement *fnStmt, bool genSymbol,
                               bool genBody)
{
    if (genSymbol)
    {
        Function *fn =
            dynamic_cast<Function *>(semanticAnalyzer.currentScope->GetSymbol(
                fnStmt->GetIdentifier()->GetLexeme(),
                fnStmt->GetCodeErrString()));

        FnDataType *fnDataType = fn->GetFnDataTypeFromFnPTR();
        llvm::FunctionType *fnType = llvm::dyn_cast<llvm::FunctionType>(
            ZtoonTypeToLLVMType(fnDataType).type);

        std::string fpName = fn->GetName();

        if (!fnStmt->IsPrototype())
        {
            if (fpName != "main")
            {
                fpName = semanticAnalyzer.currentScope->name + "::" + fpName;
            }
        }

        llvm::Function *function = llvm::Function::Create(
            fnType, llvm::GlobalValue::ExternalLinkage, fpName, *module);

        IRFunction *irFunc = gZtoonArena.Allocate<IRFunction>();
        irFunc->fn = function;
        irFunc->fnType = fnType;
        irFunc->ztoonFn = fn;
        irFunc->name = fnStmt->GetIdentifier()->GetLexeme();
        irFunc->fullName = fpName;

        AddIRSymbol(irFunc);

        debugInfo->GenFnStatementDI(fnStmt, nullptr, irFunc);
    }
    else if (genBody)
    {
        Function *fn =
            dynamic_cast<Function *>(semanticAnalyzer.currentScope->GetSymbol(
                fnStmt->GetIdentifier()->GetLexeme(),
                fnStmt->GetCodeErrString()));

        FnDataType *fnDataType = fn->GetFnDataTypeFromFnPTR();

        IRFunction *irFunc = dynamic_cast<IRFunction *>(
            GetIRSymbol(fnStmt->GetIdentifier()->GetLexeme()));
        llvm::BasicBlock *tempBB = irBuilder->GetInsertBlock();
        llvm::BasicBlock *fnBB = llvm::BasicBlock::Create(
            *ctx, std::format("{}FnBlock", fn->GetName()), irFunc->fn);
        irFunc->fnBB = fnBB;
        irBuilder->SetInsertPoint(fnBB);
        size_t index = 0;
        auto tempScope = semanticAnalyzer.currentScope;
        semanticAnalyzer.currentScope =
            semanticAnalyzer.blockToScopeMap[fnStmt->GetBlockStatement()];

        for (auto aggType : semanticAnalyzer.fnToAggDeclsMap[fn])
        {
            auto temp = semanticAnalyzer.currentScope;
            if (aggType.structStmt)
            {
                GenStructStatementIR(aggType.structStmt, true, false);
            }
        }

        for (VarDeclStatement *paramStmt : fnStmt->GetParameters())
        {
            GenVarDeclStatementIR(paramStmt, true, false);
        }

        for (auto varDecl : semanticAnalyzer.fnToVarDeclsMap[fn])
        {
            auto temp = semanticAnalyzer.currentScope;
            semanticAnalyzer.currentScope = varDecl.currentScope;
            GenVarDeclStatementIR(varDecl.varDecl, true, false);
            semanticAnalyzer.currentScope = temp;
        }

        for (VarDeclStatement *paramStmt : fnStmt->GetParameters())
        {
            GenVarDeclStatementIR(paramStmt, false, true);
            IRVariable *paramVar = dynamic_cast<IRVariable *>(
                GetIRSymbol(paramStmt->GetIdentifier()->GetLexeme()));
            llvm::Value *value = irFunc->fn->getArg(index);
            irBuilder->CreateStore(value, paramVar->value);
            index++;
        }

        GenStatementIR(fnStmt->GetBlockStatement());

        llvm::Instruction *term = irBuilder->GetInsertBlock()->getTerminator();

        if (!term || !llvm::isa<llvm::ReturnInst>(term))
        {
            if (irFunc->ztoonFn->GetFnDataTypeFromFnPTR()
                    ->returnDataType->type != DataType::Type::NOTYPE)
            {
                irBuilder->CreateRet(
                    GenExpressionIR(irFunc->ztoonFn->retStmt->GetExpression())
                        .value);
            }
            else
            {
                irBuilder->CreateRetVoid();
            }
        }
        irBuilder->SetInsertPoint(tempBB);
        semanticAnalyzer.currentScope = tempScope;
    }
}
void CodeGen::GenVarDeclStatementIR(VarDeclStatement *varDeclStmt,
                                    bool genSymbol, bool genBody)
{
    if (genSymbol)
    {
        // debugInfo->SetDebugLoc(varDeclStmt->GetIdentifier());
        UnionDataType *unionType = dynamic_cast<UnionDataType *>(
            semanticAnalyzer.stmtToDataTypeMap[varDeclStmt]);
        if (unionType)
        {
            semanticAnalyzer.stmtToDataTypeMap[varDeclStmt] =
                unionType->largestDatatype;
        }
        DataType *type = semanticAnalyzer.stmtToDataTypeMap[varDeclStmt];

        IRType varDeclType = {};
        varDeclType.type = ZtoonTypeToLLVMType(type).type;
        varDeclType.isSigned = type->IsSigned();

        Variable *var =
            dynamic_cast<Variable *>(semanticAnalyzer.currentScope->GetSymbol(
                varDeclStmt->GetIdentifier()->GetLexeme(),
                varDeclStmt->GetCodeErrString()));

        llvm::AllocaInst *inst =
            irBuilder->CreateAlloca(varDeclType.type, nullptr,
                                    varDeclStmt->GetIdentifier()->GetLexeme());
        IRVariable *irVariable = gZtoonArena.Allocate<IRVariable>();
        irVariable->value = inst;
        irVariable->variabel =
            dynamic_cast<Variable *>(semanticAnalyzer.currentScope->GetSymbol(
                varDeclStmt->GetIdentifier()->GetLexeme(),
                varDeclStmt->GetCodeErrString()));
        irVariable->variabel->dataType =
            semanticAnalyzer.stmtToDataTypeMap[varDeclStmt];
        irVariable->irType = varDeclType;
        AddIRSymbol(irVariable);
        debugInfo->GenVarDeclStatementDI(varDeclStmt, irVariable, false);
    }
    if (genBody)
    {
        auto irSymbol = GetIRSymbol(varDeclStmt->GetIdentifier()->GetLexeme());
        DataType *type = semanticAnalyzer.stmtToDataTypeMap[varDeclStmt];
        ArrayDataType *arrType = dynamic_cast<ArrayDataType *>(type);
        StructDataType *structType = dynamic_cast<StructDataType *>(type);
        auto varDeclType = ZtoonTypeToLLVMType(type);
        IRVariable *irVariable = dynamic_cast<IRVariable *>(irSymbol);
        // debugInfo->SetDebugLoc(varDeclStmt->GetIdentifier());
        if (varDeclStmt->GetExpression())
        {
            if (arrType)
            {
                std::vector<llvm::Value *> index;
                AssignValueToVarArray({irVariable->value, irVariable->irType},
                                      varDeclStmt->GetExpression(), arrType,
                                      index);
            }
            else if (structType)
            {
                AssignValueToVarStruct({irVariable->value, irVariable->irType},
                                       varDeclStmt->GetExpression(),
                                       structType);
            }
            else
            {
                IRValue value = GenExpressionIR(varDeclStmt->GetExpression());
                irBuilder->CreateStore(value.value, irVariable->value);
            }
        }
        else
        {
            if (structType)
            {
                AssignValueToVarStruct({irVariable->value, irVariable->irType},
                                       structType->defaultValuesList,
                                       structType);
            }
            else
            {
                irBuilder->CreateStore(
                    llvm::Constant::getNullValue(
                        ZtoonTypeToLLVMType(irVariable->variabel->dataType)
                            .type),
                    irVariable->value);
            }
        }
    }
}
void CodeGen::GenVarAssignmentStatementIR(
    VarAssignmentStatement *varAssignmentStmt)
{
    IRValue lValue = GenExpressionIR(varAssignmentStmt->GetLValue(), true);

    // Assign value to predefined variable.
    auto arrType = dynamic_cast<ArrayDataType *>(
        semanticAnalyzer.exprToDataTypeMap[varAssignmentStmt->GetLValue()]);
    auto structType = dynamic_cast<StructDataType *>(
        semanticAnalyzer.exprToDataTypeMap[varAssignmentStmt->GetLValue()]);
    if (arrType)
    {
        std::vector<llvm::Value *> index;
        AssignValueToVarArray(lValue, varAssignmentStmt->GetRValue(), arrType,
                              index);
    }
    else if (structType)
    {
        AssignValueToVarStruct({lValue.value, {}},
                               varAssignmentStmt->GetRValue(), structType);
    }
    else
    {
        IRValue rValue = GenExpressionIR(varAssignmentStmt->GetRValue());
        irBuilder->CreateStore(rValue.value, lValue.value);
    }
}
void CodeGen::GenVarCompundAssignmentStatementIR(
    VarCompoundAssignmentStatement *varComStmt)
{
    IRValue lValue = GenExpressionIR(varComStmt->GetLValue(), true);

    // Assign value to predefined variable.
    IRValue rValue = GenExpressionIR(varComStmt->GetRValue());

    irBuilder->CreateStore(rValue.value, lValue.value);
}
void CodeGen::GenExpressionStatementIR(ExpressionStatement *exprStmt)
{ // just evaluate the expression.
    IRValue ignore = GenExpressionIR(exprStmt->GetExpression());
}
void CodeGen::GenIfStatementIR(IfStatement *ifStmt)
{
    IRValue expression = GenExpressionIR(ifStmt->GetExpression());

    llvm::BasicBlock *currentBlock = irBuilder->GetInsertBlock();
    llvm::Function *currentFunction = currentBlock->getParent();

    llvm::BasicBlock *trueBlock =
        llvm::BasicBlock::Create(*ctx, "trueBlock", currentFunction);
    llvm::BasicBlock *falseBlock =
        llvm::BasicBlock::Create(*ctx, "falseBlock", currentFunction);
    llvm::BasicBlock *mergeBlock =
        llvm::BasicBlock::Create(*ctx, "mergeBlock", currentFunction);
    if (!irBuilder->GetInsertBlock()->getTerminator())
    {
        irBuilder->CreateCondBr(expression.value, trueBlock, falseBlock);
    }
    irBuilder->SetInsertPoint(trueBlock);

    GenStatementIR(ifStmt->GetBlockStatement());

    if (!irBuilder->GetInsertBlock()->getTerminator())
    {
        irBuilder->CreateBr(mergeBlock);
    }

    IfStatementData ifData = {};
    ifData.falseBlock = falseBlock;
    ifData.mergeBlock = mergeBlock;
    ifData.currentFunction = currentFunction;
    if (ifStmt->GetNextElseIforElseStatements().size() != 0)
    {
        for (Statement *s : ifStmt->GetNextElseIforElseStatements())
        {
            GenIfStatementIR(s, &ifData);
        }
    }
    else
    {
        irBuilder->SetInsertPoint(falseBlock);
        if (!irBuilder->GetInsertBlock()->getTerminator())
        {
            irBuilder->CreateBr(mergeBlock);
        }
    }
    if (ifData.falseBlock)
    {

        irBuilder->SetInsertPoint(ifData.falseBlock);
        if (!irBuilder->GetInsertBlock()->getTerminator())
        {
            irBuilder->CreateBr(mergeBlock);
        }
    }

    irBuilder->SetInsertPoint(mergeBlock);
}
void CodeGen::GenWhileLoopStatementIR(WhileLoopStatement *whileLoopStmt)
{
    llvm::Function *currentFunction = irBuilder->GetInsertBlock()->getParent();
    llvm::BasicBlock *loopBlock =
        llvm::BasicBlock::Create(*ctx, "loopBlock", currentFunction);
    llvm::BasicBlock *exitBlock =
        llvm::BasicBlock::Create(*ctx, "exitBlock", currentFunction);

    IRValue cond = GenExpressionIR(whileLoopStmt->GetCondition());
    if (!irBuilder->GetInsertBlock()->getTerminator())
    {
        irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);
    }

    irBuilder->SetInsertPoint(loopBlock);
    IRLoop *temp = currentLoop;
    currentLoop = gZtoonArena.Allocate<IRLoop>();
    currentLoop->loopBB = loopBlock;
    currentLoop->extBB = exitBlock;
    currentLoop->loopStmt = whileLoopStmt;

    GenStatementIR(whileLoopStmt->GetBlockStatement());
    cond = GenExpressionIR(whileLoopStmt->GetCondition());
    if (!irBuilder->GetInsertBlock()->getTerminator())
    {
        irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);
    }
    irBuilder->SetInsertPoint(exitBlock);
    currentLoop = temp;
}
void CodeGen::GenForLoopStatementIR(ForLoopStatement *forLoopStmt)
{
    GenStatementIR(forLoopStmt->GetInit());
    IRValue cond = GenExpressionIR(forLoopStmt->GetCondition());
    llvm::Function *currentFunction = irBuilder->GetInsertBlock()->getParent();
    llvm::BasicBlock *loopBlock =
        llvm::BasicBlock::Create(*ctx, "loopBlock", currentFunction);
    llvm::BasicBlock *condBlock =
        llvm::BasicBlock::Create(*ctx, "condBlock", currentFunction);
    llvm::BasicBlock *exitBlock =
        llvm::BasicBlock::Create(*ctx, "exitBlock", currentFunction);
    irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);

    irBuilder->SetInsertPoint(loopBlock);
    IRLoop *temp = currentLoop;
    currentLoop = gZtoonArena.Allocate<IRLoop>();
    currentLoop->loopBB = loopBlock;
    currentLoop->extBB = exitBlock;
    currentLoop->condBB = condBlock;
    currentLoop->loopStmt = forLoopStmt;
    GenStatementIR(forLoopStmt->GetBlockStatement());
    // in case of continue we need to do this cond.
    if (!irBuilder->GetInsertBlock()->getTerminator())
    {
        irBuilder->CreateBr(condBlock);
    }
    irBuilder->SetInsertPoint(condBlock);
    // update
    GenStatementIR(forLoopStmt->GetUpdate());
    cond = GenExpressionIR(forLoopStmt->GetCondition());
    if (!irBuilder->GetInsertBlock()->getTerminator())
    {
        irBuilder->CreateCondBr(cond.value, loopBlock, exitBlock);
    }
    irBuilder->SetInsertPoint(exitBlock);
    currentLoop = temp;
}
void CodeGen::GenBreakStatementIR(BreakStatement *breakStmt)
{
    if (!irBuilder->GetInsertBlock()->getTerminator())
    {
        irBuilder->CreateBr(currentLoop->extBB);
    }
}
void CodeGen::GenContinueStatementIR(ContinueStatement *continueStmt)
{ // condBB for 'for loop' and exitBB for 'while loop'
    if (!irBuilder->GetInsertBlock()->getTerminator())
    {
        irBuilder->CreateBr(currentLoop->condBB ? currentLoop->condBB
                                                : currentLoop->loopBB);
    }
}

void CodeGen::GenStructStatementIR(StructStatement *structStmt, bool genSymbol,
                                   bool genBody)
{
    if (structStmt->generic)
    {
        return;
    }
    auto structZtoonType = dynamic_cast<StructDataType *>(
        semanticAnalyzer.stmtToDataTypeMap[structStmt]);
    auto temp = semanticAnalyzer.currentScope;
    semanticAnalyzer.currentScope = structZtoonType->scope;
    if (genSymbol)
    {
        for (auto method : structStmt->GetMethods())
        {
            if (method->GetGeneric())
            {
                continue;
            }
            FnStatement *fnStmt = method;
            GenFnStatementIR(fnStmt, genSymbol, genBody);
        }
    }
    if (genBody)

    {
        for (auto method : structStmt->GetMethods())
        {

            if (method->GetGeneric())
            {
                continue;
            }
            FnStatement *fnStmt = method;
            GenFnStatementIR(fnStmt, genSymbol, genBody);
        }
    }

    semanticAnalyzer.currentScope = temp;
}
void CodeGen::GenUnionStatementIR(UnionStatement *unionStmt, bool genSymbol,
                                  bool genBody)
{
    if (genSymbol)
    {
        auto unionType = dynamic_cast<UnionDataType *>(
            semanticAnalyzer.stmtToDataTypeMap[unionStmt]);

        ZtoonTypeToLLVMType(unionType);
    }
}

void CodeGen::GenEnumStatementIR(EnumStatement *enumStmt, bool genSymbol,
                                 bool genBody)
{
    auto enumType = dynamic_cast<EnumDataType *>(
        semanticAnalyzer.stmtToDataTypeMap[enumStmt]);
    uint64_t uValue = 0;
    int64_t sValue = 0;
    bool useSigned = enumType->datatype->IsSigned();
    auto temp = semanticAnalyzer.currentScope;
    semanticAnalyzer.currentScope = enumType->scope;
    for (auto f : enumStmt->fields)
    {

        f->useSigned = useSigned;
        if (f->expr)
        {
            IRValue fValue = GenExpressionIR(f->expr);

            llvm::ConstantInt *valueConst =
                llvm::dyn_cast<llvm::ConstantInt>(fValue.value);
            if (!valueConst)
            {
                ReportError("Expression is not known at compile time",
                            f->expr->GetCodeErrString());
            }
            if (useSigned)
            {
                sValue = valueConst->getSExtValue();
                f->sValue = sValue;
            }
            else
            {
                uValue = valueConst->getZExtValue();
                f->uValue = uValue;
            }
        }
        else
        {
            if (useSigned)
            {

                f->sValue = ++sValue;
            }
            else
            {

                f->uValue = ++uValue;
            }
        }
        IRReadonlySymbol *irReadonlySymbol =
            gZtoonArena.Allocate<IRReadonlySymbol>();
        irReadonlySymbol->name = f->identifier->GetLexeme();
        irReadonlySymbol->irType = ZtoonTypeToLLVMType(enumType->GetDataType());
        if (useSigned)
        {
            irReadonlySymbol->value = llvm::ConstantInt::get(
                irReadonlySymbol->irType.type, f->sValue);
        }
        else
        {
            irReadonlySymbol->value = llvm::ConstantInt::get(
                irReadonlySymbol->irType.type, f->uValue);
        }

        AddIRSymbol(irReadonlySymbol);
    }
    semanticAnalyzer.currentScope = temp;
}
void CodeGen::GenRetStatementIR(RetStatement *retStmt)
{
    // debugInfo->SetDebugLoc(retStmt->retToken);
    if (auto t = irBuilder->GetInsertBlock()->getTerminator())
    {
        if (llvm::isa<llvm::ReturnInst>(t))
        {
            return;
        }
    }

    if (semanticAnalyzer.stmtToDataTypeMap[retStmt]->GetType() !=
        DataType::Type::NOTYPE)
    {
        IRValue retValue = GenExpressionIR(retStmt->GetExpression());
        irBuilder->CreateRet(retValue.value);
    }
    else
    {
        irBuilder->CreateRetVoid();
    }
}

void CodeGen::GenIfStatementIR(Statement *statement, IfStatementData *ifData)
{
    if (dynamic_cast<ElseIfStatement *>(statement))
    {
        ElseIfStatement *elifStatement =
            dynamic_cast<ElseIfStatement *>(statement);
        assert(ifData != nullptr);

        irBuilder->SetInsertPoint(ifData->falseBlock);

        IRValue expression = GenExpressionIR(elifStatement->GetExpression());
        llvm::BasicBlock *trueBlock = llvm::BasicBlock::Create(
            *ctx, "trueBlock", ifData->currentFunction);
        llvm::BasicBlock *falseBlock = llvm::BasicBlock::Create(
            *ctx, "falseBlock", ifData->currentFunction);
        irBuilder->CreateCondBr(expression.value, trueBlock, falseBlock);
        irBuilder->SetInsertPoint(trueBlock);

        GenStatementIR(elifStatement->GetBlockStatement());

        if (!irBuilder->GetInsertBlock()->getTerminator())
        {
            irBuilder->CreateBr(ifData->mergeBlock);
        }

        // Prepare for next else if or else
        ifData->falseBlock = falseBlock;
    }
    else if (dynamic_cast<ElseStatement *>(statement))
    {
        ElseStatement *elseStatement = dynamic_cast<ElseStatement *>(statement);
        assert(ifData != nullptr);

        llvm::BasicBlock *trueBlock = llvm::BasicBlock::Create(
            *ctx, "trueBlock", ifData->currentFunction);
        irBuilder->SetInsertPoint(ifData->falseBlock);
        irBuilder->CreateBr(trueBlock);

        irBuilder->SetInsertPoint(trueBlock);

        GenStatementIR(elseStatement->GetBlockStatement());

        if (!irBuilder->GetInsertBlock()->getTerminator())
        {
            irBuilder->CreateBr(ifData->mergeBlock);
        }

        // exit
        ifData->falseBlock = nullptr;
    }
}

bool CodeGen::IsNaN(llvm::Value *value)
{
    if (value->getType()->isFloatingPointTy())
    {
        llvm::ConstantFP *fp = llvm::dyn_cast<llvm::ConstantFP>(value);
        if (fp)
        {
            return fp->isNaN();
        }
    }
    return false;
}
llvm::CmpInst::Predicate
CodeGen::CMPZtoonTypeToCMPLLVM(TokenType type, IRValue value, bool isNaN)
{
    bool isSigned = value.type.isSigned;
    bool isInteger = value.value->getType()->isIntegerTy() ||
                     value.value->getType()->isPointerTy();
    switch (type)
    {
    case TokenType::EQUAL_EQUAL:
    {
        if (isInteger)
        {
            return llvm::CmpInst::Predicate::ICMP_EQ;
        }
        else
        {
            return isNaN ? llvm::CmpInst::Predicate::FCMP_UEQ
                         : llvm::CmpInst::Predicate::FCMP_OEQ;
        }
    }
    case TokenType::EXCLAMATION_EQUAL:
    {
        if (isInteger)
        {
            return llvm::CmpInst::Predicate::ICMP_NE;
        }
        else
        {
            return isNaN ? llvm::CmpInst::Predicate::FCMP_UNE
                         : llvm::CmpInst::Predicate::FCMP_ONE;
        }
    }
    case TokenType::LESS:
    {
        if (isInteger)
        {
            return isSigned ? llvm::CmpInst::Predicate::ICMP_SLT
                            : llvm::CmpInst::Predicate::ICMP_ULT;
        }
        else
        {
            return isNaN ? llvm::CmpInst::Predicate::FCMP_ULT
                         : llvm::CmpInst::Predicate::FCMP_OLT;
        }
    }
    case TokenType::LESS_EQUAL:
    {
        if (isInteger)
        {
            return isSigned ? llvm::CmpInst::Predicate::ICMP_SLE
                            : llvm::CmpInst::Predicate::ICMP_ULE;
        }
        else
        {
            return isNaN ? llvm::CmpInst::Predicate::FCMP_ULE
                         : llvm::CmpInst::Predicate::FCMP_OLE;
        }
    }
    case TokenType::GREATER:
    {
        if (isInteger)
        {
            return isSigned ? llvm::CmpInst::Predicate::ICMP_SGT
                            : llvm::CmpInst::Predicate::ICMP_UGT;
        }
        else
        {
            return isNaN ? llvm::CmpInst::Predicate::FCMP_UGT
                         : llvm::CmpInst::Predicate::FCMP_OGT;
        }
    }
    case TokenType::GREATER_EQUAL:
    {
        if (isInteger)
        {
            return isSigned ? llvm::CmpInst::Predicate::ICMP_SGE
                            : llvm::CmpInst::Predicate::ICMP_UGE;
        }
        else
        {
            return isNaN ? llvm::CmpInst::Predicate::FCMP_UGE
                         : llvm::CmpInst::Predicate::FCMP_OGE;
        }
    }
    default:
        return llvm::CmpInst::Predicate::FCMP_FALSE;
    }
}
IRValue CodeGen::GenFnExpressionIR(FnExpression *fnExpr, bool isWrite)
{
    Function *fn =
        dynamic_cast<Function *>(semanticAnalyzer.currentScope->GetSymbol(
            fnExpr->GetName(), fnExpr->GetCodeErrString()));

    FnDataType *fnDataType = fn->GetFnDataTypeFromFnPTR();
    llvm::FunctionType *fnType = llvm::dyn_cast<llvm::FunctionType>(
        ZtoonTypeToLLVMType(fnDataType).type);
    std::string fpName = fn->GetName();

    if (!fnExpr->IsPrototype())
    {
        fpName = semanticAnalyzer.currentScope->name + "::" + fpName;
    }
    llvm::Function *function = llvm::Function::Create(
        fnType, llvm::GlobalValue::ExternalLinkage, fpName, *module);

    IRFunction *irFunc = gZtoonArena.Allocate<IRFunction>();
    irFunc->fn = function;
    irFunc->fnType = fnType;
    irFunc->ztoonFn = fn;
    irFunc->name = fn->GetName();
    irFunc->fullName = fpName;

    AddIRSymbol(irFunc);
    debugInfo->GenFnStatementDI(nullptr, fnExpr, irFunc);
    // Go out of current block, save it then set it back;
    llvm::BasicBlock *tempBB = irBuilder->GetInsertBlock();
    llvm::BasicBlock *fnBB = llvm::BasicBlock::Create(
        *ctx, std::format("{}FnBlock", fn->GetName()), irFunc->fn);
    irFunc->fnBB = fnBB;
    irBuilder->SetInsertPoint(fnBB);

    auto tempScope = semanticAnalyzer.currentScope;
    semanticAnalyzer.currentScope =
        semanticAnalyzer.blockToScopeMap[fnExpr->GetBlockStatement()];
    size_t index = 0;

    for (auto aggType : semanticAnalyzer.fnToAggDeclsMap[fn])
    {
        if (aggType.structStmt)
        {
            GenStructStatementIR(aggType.structStmt, true, false);
        }
    }
    for (VarDeclStatement *paramStmt : fnExpr->GetParameters())
    {
        GenVarDeclStatementIR(paramStmt, true, false);
    }

    for (auto varDecl : semanticAnalyzer.fnToVarDeclsMap[fn])
    {
        auto temp = semanticAnalyzer.currentScope;
        semanticAnalyzer.currentScope = varDecl.currentScope;
        GenVarDeclStatementIR(varDecl.varDecl, true, false);
        semanticAnalyzer.currentScope = temp;
    }

    for (VarDeclStatement *paramStmt : fnExpr->GetParameters())
    {
        GenVarDeclStatementIR(paramStmt, false, true);
        IRVariable *paramVar = dynamic_cast<IRVariable *>(
            GetIRSymbol(paramStmt->GetIdentifier()->GetLexeme()));
        llvm::Value *value = irFunc->fn->getArg(index);
        irBuilder->CreateStore(value, paramVar->value);
        index++;
    }
    GenStatementIR(fnExpr->GetBlockStatement());

    llvm::Instruction *term = irBuilder->GetInsertBlock()->getTerminator();

    if (!term || !llvm::isa<llvm::ReturnInst>(term))
    {
        if (irFunc->ztoonFn->GetFnDataTypeFromFnPTR()->returnDataType->type !=
            DataType::Type::NOTYPE)
        {
            irBuilder->CreateRet(
                GenExpressionIR(irFunc->ztoonFn->retStmt->GetExpression())
                    .value);
        }
        else
        {
            irBuilder->CreateRetVoid();
        }
    }
    semanticAnalyzer.currentScope = tempScope;
    irBuilder->SetInsertPoint(tempBB);
    IRValue irValue;
    irValue.value = irFunc->GetValue();
    irValue.type.type = irFunc->GetType();
    return irValue;
}
IRValue CodeGen::GenTernaryExpressionIR(TernaryExpression *ternaryExpr,
                                        bool isWrite)
{
    IRValue irValue;
    IRValue condition = GenExpressionIR(ternaryExpr->GetCondition());

    llvm::Function *currentFunction = irBuilder->GetInsertBlock()->getParent();
    llvm::BasicBlock *trueBlock =
        llvm::BasicBlock::Create(*ctx, "trueBlock", currentFunction);
    llvm::BasicBlock *falseBlock =
        llvm::BasicBlock::Create(*ctx, "falseBlock", currentFunction);
    llvm::BasicBlock *mergeBlock =
        llvm::BasicBlock::Create(*ctx, "mergeBlock", currentFunction);

    irBuilder->CreateCondBr(condition.value, trueBlock, falseBlock);

    irBuilder->SetInsertPoint(trueBlock);

    IRValue trueValue = GenExpressionIR(ternaryExpr->GetTrueExpression());
    irBuilder->CreateBr(mergeBlock);

    irBuilder->SetInsertPoint(falseBlock);

    IRValue falseValue = GenExpressionIR(ternaryExpr->GetFalseExpression());

    irBuilder->CreateBr(mergeBlock);

    irBuilder->SetInsertPoint(mergeBlock);
    llvm::PHINode *phi = irBuilder->CreatePHI(trueValue.type.type, 2, "result");
    phi->addIncoming(trueValue.value, trueBlock);
    phi->addIncoming(falseValue.value, falseBlock);
    irValue.value = phi;
    irValue.type = trueValue.type;

    return irValue;
}
IRValue CodeGen::GenBinaryExpressionIR(BinaryExpression *binaryExpr,
                                       bool isWrite)
{
    IRValue irValue;
    IRValue lValue = GenExpressionIR(binaryExpr->GetLeftExpression());
    IRValue rValue = GenExpressionIR(binaryExpr->GetRightExpression());
    DataType *binExprDataType = semanticAnalyzer.exprToDataTypeMap[binaryExpr];
    DataType *rightExprDataType =
        semanticAnalyzer.exprToDataTypeMap[binaryExpr->GetRightExpression()];
    DataType *leftExprDataType =
        semanticAnalyzer.exprToDataTypeMap[binaryExpr->GetLeftExpression()];
    irValue.type = ZtoonTypeToLLVMType(binExprDataType);
    switch (binaryExpr->GetOperator()->GetType())
    {
    case TokenType::PLUS:
    {
        if (binExprDataType->IsInteger())
        {
            irValue.value = irBuilder->CreateAdd(
                lValue.value, rValue.value,
                std::format("bin_op_{}",
                            binaryExpr->GetOperator()->GetLexeme()));
        }
        else if (binExprDataType->IsFloat())
        {

            irValue.value = irBuilder->CreateFAdd(
                lValue.value, rValue.value,
                std::format("bin_op_{}",
                            binaryExpr->GetOperator()->GetLexeme()));
        }

        break;
    }
    case TokenType::DASH:
    {
        if (binExprDataType->IsInteger())
        {
            irValue.value = irBuilder->CreateSub(
                lValue.value, rValue.value,
                std::format("bin_op_{}",
                            binaryExpr->GetOperator()->GetLexeme()));
        }
        else if (binExprDataType->IsFloat())
        {

            irValue.value = irBuilder->CreateFSub(
                lValue.value, rValue.value,
                std::format("bin_op_{}",
                            binaryExpr->GetOperator()->GetLexeme()));
        }
        break;
    }
    case TokenType::ASTERISK:
    {
        if (binExprDataType->IsInteger())
        {
            irValue.value = irBuilder->CreateMul(
                lValue.value, rValue.value,
                std::format("bin_op_{}",
                            binaryExpr->GetOperator()->GetLexeme()));
        }
        else if (binExprDataType->IsFloat())
        {

            irValue.value = irBuilder->CreateFMul(
                lValue.value, rValue.value,
                std::format("bin_op_{}",
                            binaryExpr->GetOperator()->GetLexeme()));
        }
        break;
    }
    case TokenType::SLASH:
    {

        if (binExprDataType->IsInteger())
        {
            irValue.value =
                irValue.type.isSigned
                    ? irBuilder->CreateSDiv(
                          lValue.value, rValue.value,
                          std::format("bin_op_{}",
                                      binaryExpr->GetOperator()->GetLexeme()))
                    : irBuilder->CreateUDiv(
                          lValue.value, rValue.value,
                          std::format("bin_op_{}",
                                      binaryExpr->GetOperator()->GetLexeme()));
        }
        else if (binExprDataType->IsFloat())
        {

            irValue.value = irBuilder->CreateFDiv(
                lValue.value, rValue.value,
                std::format("bin_op_{}",
                            binaryExpr->GetOperator()->GetLexeme()));
        }
        break;
    }
    case TokenType::PERCENTAGE:
    {

        if (binExprDataType->IsInteger())
        {
            irValue.value =
                lValue.type.isSigned
                    ? irBuilder->CreateSRem(
                          lValue.value, rValue.value,
                          std::format("bin_op_{}",
                                      binaryExpr->GetOperator()->GetLexeme()))
                    : irBuilder->CreateURem(
                          lValue.value, rValue.value,
                          std::format("bin_op_{}",
                                      binaryExpr->GetOperator()->GetLexeme()));
        }
        else if (binExprDataType->IsFloat())
        {
            irValue.value = irBuilder->CreateFRem(
                lValue.value, rValue.value,
                std::format("bin_op_{}",
                            binaryExpr->GetOperator()->GetLexeme()));
        }
        break;
    }
    case TokenType::BITWISE_AND:
    {
        irValue.value = irBuilder->CreateAnd(
            lValue.value, rValue.value,
            std::format("bin_op_{}", binaryExpr->GetOperator()->GetLexeme()));
        break;
    }
    case TokenType::BITWISE_OR:
    {
        irValue.value = irBuilder->CreateOr(
            lValue.value, rValue.value,
            std::format("bin_op_{}", binaryExpr->GetOperator()->GetLexeme()));
        break;
    }
    case TokenType::BITWISE_XOR:
    {
        irValue.value = irBuilder->CreateXor(
            lValue.value, rValue.value,
            std::format("bin_op_{}", binaryExpr->GetOperator()->GetLexeme()));
        break;
    }
    case TokenType::SHIFT_LEFT:
    {

        irValue.value = irBuilder->CreateShl(
            lValue.value, rValue.value,
            std::format("bin_op_{}", binaryExpr->GetOperator()->GetLexeme()));
        break;
    }
    case TokenType::SHIFT_RIGHT:
    {
        irValue.value =
            lValue.type.isSigned
                ? irBuilder->CreateAShr(
                      lValue.value, rValue.value,
                      std::format("bin_op_{}",
                                  binaryExpr->GetOperator()->GetLexeme()))
                : irBuilder->CreateLShr(
                      lValue.value, rValue.value,
                      std::format("bin_op_{}",
                                  binaryExpr->GetOperator()->GetLexeme()));
        break;
    }
    case TokenType::EQUAL_EQUAL:
    case TokenType::EXCLAMATION_EQUAL:
    case TokenType::LESS:
    case TokenType::LESS_EQUAL:
    case TokenType::GREATER:
    case TokenType::GREATER_EQUAL:
    {
        if (!leftExprDataType->IsNumerical() &&
            leftExprDataType->GetType() != DataType::Type::BOOL)
        {
            ReportError(std::format("Left expression of binary operator '{}' "
                                    "cannot be of type '{}'",
                                    binaryExpr->GetOperator()->GetLexeme(),
                                    leftExprDataType->ToString()),
                        binaryExpr->GetCodeErrString());
        }
        else if (!rightExprDataType->IsNumerical() &&
                 rightExprDataType->GetType() != DataType::Type::BOOL)
        {
            ReportError(std::format("Right expression of binary operator '{}' "
                                    "cannot be of type '{}'",
                                    binaryExpr->GetOperator()->GetLexeme(),
                                    rightExprDataType->ToString()),
                        binaryExpr->GetCodeErrString());
        }
        else
        {
            irValue.value = irBuilder->CreateCmp(
                CMPZtoonTypeToCMPLLVM(
                    binaryExpr->GetOperator()->GetType(), lValue,
                    IsNaN(lValue.value) || IsNaN(rValue.value)),
                lValue.value, rValue.value,
                std::format("bin_op_{}",
                            binaryExpr->GetOperator()->GetLexeme()));

            break;
        }
    }
    case TokenType::OR:
    {
        if (!leftExprDataType->IsNumerical() &&
            leftExprDataType->GetType() != DataType::Type::BOOL)
        {
            ReportError(std::format("Left expression of binary operator '{}' "
                                    "cannot be of type '{}'",
                                    binaryExpr->GetOperator()->GetLexeme(),
                                    leftExprDataType->ToString()),
                        binaryExpr->GetCodeErrString());
        }
        else if (!!rightExprDataType->IsNumerical() &&
                 rightExprDataType->GetType() != DataType::Type::BOOL)
        {
            ReportError(std::format("Right expression of binary operator '{}' "
                                    "cannot be of type '{}'",
                                    binaryExpr->GetOperator()->GetLexeme(),
                                    rightExprDataType->ToString()),
                        binaryExpr->GetCodeErrString());
        }
        else
        {
            irValue.value = irBuilder->CreateOr(
                lValue.value, rValue.value,
                std::format("bin_op_{}",
                            binaryExpr->GetOperator()->GetLexeme()));
            break;
        }
        break;
    }
    case TokenType::AND:
    {
        if (!leftExprDataType->IsNumerical() &&
            leftExprDataType->GetType() != DataType::Type::BOOL)
        {
            ReportError(std::format("Left expression of binary operator '{}' "
                                    "cannot be of type '{}'",
                                    binaryExpr->GetOperator()->GetLexeme(),
                                    leftExprDataType->ToString()),
                        binaryExpr->GetCodeErrString());
        }
        else if (!!rightExprDataType->IsNumerical() &&
                 rightExprDataType->GetType() != DataType::Type::BOOL)
        {
            ReportError(std::format("Right expression of binary operator '{}' "
                                    "cannot be of type '{}'",
                                    binaryExpr->GetOperator()->GetLexeme(),
                                    rightExprDataType->ToString()),
                        binaryExpr->GetCodeErrString());
        }
        else
        {
            irValue.value = irBuilder->CreateAnd(
                lValue.value, rValue.value,
                std::format("bin_op_{}",
                            binaryExpr->GetOperator()->GetLexeme()));
            break;
        }
        break;
    }
    default:
    {
        ReportError(std::format("Binary operator '{}' is not supported.",
                                binaryExpr->GetOperator()->GetLexeme()),
                    binaryExpr->GetCodeErrString());
        break;
    }
    }

    return irValue;
}
IRValue CodeGen::GenCastExpressionIR(CastExpression *castExpr, bool isWrite)
{
    IRValue irValue;
    DataType *castDataType = semanticAnalyzer.exprToDataTypeMap[castExpr];
    DataType *valueType =
        semanticAnalyzer.exprToDataTypeMap[castExpr->GetExpression()];
    IRType castType = ZtoonTypeToLLVMType(castDataType);
    IRType originalType = ZtoonTypeToLLVMType(valueType);

    bool arrToPtrCast = false;
    if (valueType->GetType() == DataType::Type::ARRAY &&
        castDataType->GetType() == DataType::Type::POINTER)
    {
        arrToPtrCast = true;
    }

    IRValue toCastValue =
        GenExpressionIR(castExpr->GetExpression(), arrToPtrCast);

    if (toCastValue.type.type->isArrayTy())
    {
        toCastValue.value = irBuilder->CreateInBoundsGEP(
            toCastValue.type.type, toCastValue.value,
            {irBuilder->getInt32(0), irBuilder->getInt32(0)});
    }

    if (castType.type->isIntegerTy())
    {
        size_t size = castType.type->getIntegerBitWidth();
        if (size == 1)
        {
            llvm::Value *value = toCastValue.value;
            if (toCastValue.value->getType()->isPointerTy())
            {
                value = irBuilder->CreateLoad(
                    irBuilder->getIntNTy(this->GetPtrBitWidth()),
                    toCastValue.value);
            }
            toCastValue.value = irBuilder->CreateCmp(
                llvm::CmpInst::Predicate::ICMP_NE, value,
                irBuilder->getIntN(value->getType()->getIntegerBitWidth(), 0));
        }
    }

    irValue = CastIRValue(toCastValue, castType);
    return irValue;
}
IRValue CodeGen::GenGroupingExpressionIR(GroupingExpression *groupingEpxr,
                                         bool isWrite)
{
    return GenExpressionIR(groupingEpxr->GetExpression(), isWrite);
}
IRValue CodeGen::GenSubScriptExpressionIR(SubscriptExpression *subExpr,
                                          bool isWrite)
{
    IRValue irValue;
    auto exprDataType = dynamic_cast<ArrayDataType *>(
        semanticAnalyzer.exprToDataTypeMap[subExpr->GetExpression()]);

    IRValue ptr = GenExpressionIR(subExpr->GetExpression(), exprDataType);
    IRValue index = GenExpressionIR(subExpr->GetIndexExpression());
    irValue.type =
        ZtoonTypeToLLVMType(semanticAnalyzer.exprToDataTypeMap[subExpr]);
    if (isWrite && ptr.value->getType()->isArrayTy())
    {
        assert(0);
    }
    if (exprDataType && !ptr.value->getType()->isArrayTy())
    {
        ptr.value = irBuilder->CreateInBoundsGEP(
            ZtoonTypeToLLVMType(exprDataType).type, ptr.value,
            {irBuilder->getInt32(0), irBuilder->getInt32(0)});
        ptr.type.type = ptr.value->getType();
    }
    else if (ptr.value->getType()->isArrayTy())
    {
        llvm::Value *arrAlloca = irBuilder->CreateAlloca(
            ZtoonTypeToLLVMType(exprDataType).type, nullptr,
            subExpr->GetExpression()->GetCodeErrString().str);
        irBuilder->CreateStore(ptr.value, arrAlloca);
        ptr.value = irBuilder->CreateInBoundsGEP(
            ZtoonTypeToLLVMType(exprDataType).type, arrAlloca,
            {irBuilder->getInt32(0), irBuilder->getInt32(0)});
        ptr.type.type = ptr.value->getType();
    }

    llvm::Value *GEP = irBuilder->CreateInBoundsGEP(
        irValue.type.type, ptr.value, index.value,
        std::format("ptr__{}__at_index__{}",
                    subExpr->GetExpression()->GetCodeErrString().str,
                    subExpr->GetIndexExpression()->GetCodeErrString().str));

    if (isWrite)
    {
        irValue.value = GEP;
    }
    else
    {
        irValue.value = irBuilder->CreateLoad(
            irValue.type.type, GEP,
            std::format("{}__at_index__{}",
                        subExpr->GetExpression()->GetCodeErrString().str,
                        subExpr->GetIndexExpression()->GetCodeErrString().str));
    }

    return irValue;
}
IRValue CodeGen::GenUnaryExpressionIR(UnaryExpression *unaryExpr, bool isWrite)
{
    IRValue irValue;
    IRValue rValue;
    if (!unaryExpr->GetSizeOfDataTypeToken() &&
        unaryExpr->GetOperator()->GetType() != TokenType::ASTERISK &&
        unaryExpr->GetOperator()->GetType() != TokenType::BITWISE_AND)
    {

        rValue = GenExpressionIR(unaryExpr->GetRightExpression(), isWrite);

        irValue.type = rValue.type;
    }

    DataType *unaryDataType = semanticAnalyzer.exprToDataTypeMap[unaryExpr];
    switch (unaryExpr->GetOperator()->GetType())
    {
    case TokenType::DASH:
    {
        irValue.type.isSigned = !irValue.type.isSigned;
        if (unaryDataType->IsInteger())
        {

            irValue.value = irBuilder->CreateNeg(
                rValue.value,
                std::format("unary_op_{}",
                            unaryExpr->GetOperator()->GetLexeme()));
        }
        else if (unaryDataType->IsFloat())
        {
            irValue.value = irBuilder->CreateFNeg(
                rValue.value,
                std::format("unary_op_{}",
                            unaryExpr->GetOperator()->GetLexeme()));
        }
        break;
    }
    case TokenType::TILDE:
    {
        if (unaryDataType->IsInteger() || unaryDataType->IsFloat())
        {
            irValue.value = irBuilder->CreateXor(
                rValue.value,
                llvm::ConstantInt::get(rValue.value->getType(), -1),
                std::format("unary_op_{}",
                            unaryExpr->GetOperator()->GetLexeme()));
        }
        break;
    }
    case TokenType::DASH_DASH:
    {
        // rvalue here is unary right expression, not meaning the expr
        // is right value expression. my brain.
        IRValue lValue = GenExpressionIR(unaryExpr->GetRightExpression());
        rValue.value = irBuilder->CreateLoad(lValue.type.type, lValue.value);
        rValue.type = lValue.type;
        irValue.type = rValue.type;
        if (unaryDataType->IsInteger())
        {
            // Get the variable
            irValue.value = irBuilder->CreateSub(
                rValue.value,
                llvm::ConstantInt::get(rValue.value->getType(), 1),
                std::format("unary_op_{}",
                            unaryExpr->GetOperator()->GetLexeme()));
        }
        else if (unaryDataType->IsFloat())
        {
            irValue.value = irBuilder->CreateFSub(
                rValue.value,
                llvm::ConstantInt::get(rValue.value->getType(), 1.0),
                std::format("unary_op_{}",
                            unaryExpr->GetOperator()->GetLexeme()));
        }
        irBuilder->CreateStore(irValue.value, lValue.value);
        break;
    }
    case TokenType::PLUS:
    {
        irValue = rValue;
        break;
    }
    case TokenType::PLUS_PLUS:
    {

        // rvalue here is unary right expression, not meaning the expr
        // is right value expression. my brain.
        IRValue lValue = GenExpressionIR(unaryExpr->GetRightExpression());
        rValue.value = irBuilder->CreateLoad(lValue.type.type, lValue.value);
        rValue.type = lValue.type;
        irValue.type = rValue.type;
        if (unaryDataType->IsInteger())
        {

            irValue.value = irBuilder->CreateAdd(
                rValue.value,
                llvm::ConstantInt::get(rValue.value->getType(), 1),
                std::format("unary_op_{}",
                            unaryExpr->GetOperator()->GetLexeme()));
        }
        else if (unaryDataType->IsFloat())
        {
            irValue.value = irBuilder->CreateFAdd(
                rValue.value,
                llvm::ConstantInt::get(rValue.value->getType(), 1.0),
                std::format("unary_op_{}",
                            unaryExpr->GetOperator()->GetLexeme()));
        }
        irBuilder->CreateStore(irValue.value, lValue.value);
        break;
    }
    case TokenType::EXCLAMATION:
    {
        // numerical
        irValue.value = irBuilder->CreateXor(
            rValue.value, llvm::ConstantInt::getTrue(*ctx),
            std::format("unary_op_{}", unaryExpr->GetOperator()->GetLexeme()));
        break;
    }
    case TokenType::SIZEOF:
    {
        irValue.type = ZtoonTypeToLLVMType(
            semanticAnalyzer.currentScope->datatypesMap["u64"]);
        uint64_t size = moduleDataLayout->getTypeAllocSize(
            rValue.value ? rValue.value->getType()
                         : ZtoonTypeToLLVMType(
                               semanticAnalyzer.currentScope->GetDataType(
                                   unaryExpr->GetSizeOfDataTypeToken()))
                               .type);
        irValue.value =
            llvm::ConstantInt::get(llvm::Type::getInt64Ty(*ctx), size);
        break;
    }
    case TokenType::ASTERISK:
    {
        PointerDataType *exprPtr = dynamic_cast<PointerDataType *>(
            semanticAnalyzer
                .exprToDataTypeMap[unaryExpr->GetRightExpression()]);
        rValue = GenExpressionIR(unaryExpr->GetRightExpression(), false);

        rValue.type = ZtoonTypeToLLVMType(unaryDataType);
        if (!isWrite)
        {
            rValue.value = irBuilder->CreateLoad(
                rValue.type.type, rValue.value,
                std::format(
                    "deref_{}",
                    unaryExpr->GetRightExpression()->GetCodeErrString().str));
        }

        irValue = rValue;
        break;
    }
    case TokenType::BITWISE_AND:
    {
        irValue = GenExpressionIR(unaryExpr->GetRightExpression(), true);

        break;
    }
    default:
    {
        ReportError(std::format("Unkown unary operator '{}'",
                                unaryExpr->GetOperator()->GetLexeme()),
                    unaryExpr->GetCodeErrString());
        break;
    }
    }
    return irValue;
}
IRValue CodeGen::GenFnCallExpressionIR(FnCallExpression *fnCallExpr,
                                       bool isWrite)
{
    // debugInfo->SetDebugLoc(fnCallExpr->GetFirstToken());
    IRValue exprValue = GenExpressionIR(fnCallExpr->GetGetExpression());

    auto fnPtrType = dynamic_cast<PointerDataType *>(
        semanticAnalyzer.exprToDataTypeMap[fnCallExpr->GetGetExpression()]);
    auto fnType = dynamic_cast<FnDataType *>(fnPtrType->PointedToDatatype());
    std::vector<llvm::Value *> args;
    size_t index = 0;
    for (auto *arg : fnCallExpr->GetArgs())
    {
        IRValue argIrValue = GenExpressionIR(arg);

        ArrayDataType *paramArrayType;
        if (index < fnType->GetParameters().size())
        {
            paramArrayType =
                dynamic_cast<ArrayDataType *>(fnType->GetParameters()[index]);
        }
        else
        {
            if (fnType->IsVarArgs())
            {
                paramArrayType = dynamic_cast<ArrayDataType *>(
                    semanticAnalyzer.exprToDataTypeMap[arg]);
            }
            else
            {
                ReportError("Arguments are not compatible with "
                            "function parameters",
                            fnCallExpr->GetCodeErrString());
            }
        }

        // Copy array "pass by value"
        if (paramArrayType)
        {
            llvm::Value *arrAlloca = irBuilder->CreateAlloca(
                ZtoonTypeToLLVMType(paramArrayType).type, nullptr,
                arg->GetCodeErrString().str);

            std::vector<llvm::Value *> index;
            index.push_back(irBuilder->getInt32(0));
            AssignValueToVarArray(
                {arrAlloca, ZtoonTypeToLLVMType(paramArrayType->dataType)}, arg,
                paramArrayType, index);
            argIrValue.value = irBuilder->CreateLoad(
                ZtoonTypeToLLVMType(paramArrayType).type, arrAlloca);
        }

        args.push_back(argIrValue.value);

        index++;
    }
    // Get fnPointerDatatype
    auto retDataType = dynamic_cast<DataType *>(
        semanticAnalyzer.exprToDataTypeMap[fnCallExpr]);
    IRType fnIrType = ZtoonTypeToLLVMType(fnType);
    llvm::FunctionType *fnIrTypeCast =
        llvm::cast<llvm::FunctionType>(fnIrType.type);
    IRValue irValue;
    irValue.type = ZtoonTypeToLLVMType(retDataType);
    irValue.value = irBuilder->CreateCall(
        fnIrTypeCast, exprValue.value, args,
        retDataType->type == DataType::Type::NOTYPE
            ? ""
            : std::format(
                  "{}_call",
                  fnCallExpr->GetGetExpression()->GetCodeErrString().str));
    return irValue;
}
IRValue CodeGen::GenMemberAccessExpressionIR(MemberAccessExpression *maExpr,
                                             bool isWrite)
{
    IRValue irValue = {};
    if (maExpr->accessType == MemberAccessExpression::AccessType::STRUCT)
    {
        auto type =
            semanticAnalyzer.exprToDataTypeMap[maExpr->GetLeftExpression()];

        StructDataType *structType = dynamic_cast<StructDataType *>(type);
        PointerDataType *ptrType = dynamic_cast<PointerDataType *>(type);

        bool nonMethodCall = false;
        if (structType)
        {
            auto leftPrimaryExpr =
                dynamic_cast<PrimaryExpression *>(maExpr->GetLeftExpression());
            if (leftPrimaryExpr)
            {
                if (leftPrimaryExpr->GetPrimary()->GetLexeme() ==
                        structType->GetName() &&
                    maExpr->token->GetType() == TokenType::DOUBLE_COLON)
                {
                    nonMethodCall = true;
                }
            }
        }

        IRValue leftValue =
            nonMethodCall
                ? (IRValue){}
                : GenExpressionIR(maExpr->GetLeftExpression(), !ptrType);

        if (ptrType)
        {
            structType = dynamic_cast<StructDataType *>(ptrType->dataType);
        }

        size_t index = 0;
        PrimaryExpression *primaryExpr =
            dynamic_cast<PrimaryExpression *>(maExpr->GetRightExpression());
        MemberAccessExpression *ma = dynamic_cast<MemberAccessExpression *>(
            maExpr->GetRightExpression());

        std::string rightName = primaryExpr->GetPrimary()->GetLexeme();
        IRType fieldIRType = {};
        bool fieldFound = false;
        for (size_t i = 0; i < structType->structStmt->GetFields().size(); i++)
        {
            if (rightName == structType->structStmt->GetFields()[i]
                                 ->GetIdentifier()
                                 ->GetLexeme())
            {
                index = i;
                fieldIRType = ZtoonTypeToLLVMType(structType->fields[index]);
                fieldFound = true;
                break;
            }
        }
        if (fieldFound && !nonMethodCall)
        {
            IRType structIRType = ZtoonTypeToLLVMType(structType);
            irValue.value = irBuilder->CreateStructGEP(structIRType.type,
                                                       leftValue.value, index);
            irValue.type = fieldIRType;

            if (!isWrite)
            {
                irValue.value =
                    irBuilder->CreateLoad(fieldIRType.type, irValue.value);
            }
        }
        else
        {
            auto temp = semanticAnalyzer.currentScope;
            semanticAnalyzer.currentScope = structType->scope;
            irValue = GenExpressionIR(maExpr->GetRightExpression());
            semanticAnalyzer.currentScope = temp;
        }
    }
    else if (maExpr->accessType == MemberAccessExpression::AccessType::UNION)
    {
        IRValue value = GenExpressionIR(maExpr->GetLeftExpression(), true);
        // casting
        auto castToType = semanticAnalyzer.exprToDataTypeMap[maExpr];
        auto castToPtrType = gZtoonArena.Allocate<PointerDataType>();
        castToPtrType->type = DataType::Type::POINTER;
        castToPtrType->dataType = castToType;

        value = CastPtrToPtr(value, ZtoonTypeToLLVMType(castToPtrType));

        IRType type = ZtoonTypeToLLVMType(castToType);
        value.type = type;

        if (!isWrite)
        {
            value.value = irBuilder->CreateLoad(value.type.type, value.value);
        }

        irValue = value;
    }
    else if (maExpr->accessType == MemberAccessExpression::AccessType::ENUM)
    {
        auto enumType = dynamic_cast<EnumDataType *>(
            semanticAnalyzer.exprToDataTypeMap[maExpr]);
        IRType type = ZtoonTypeToLLVMType(enumType);
        auto enumStmt = enumType->enumStmt;
        auto primaryExpr =
            dynamic_cast<PrimaryExpression *>(maExpr->GetRightExpression());

        EnumStatement::Field *enumField = nullptr;
        for (auto f : enumStmt->fields)
        {
            if (f->identifier->GetLexeme() == primaryExpr->primary->GetLexeme())
            {
                enumField = f;
                break;
            }
        }

        llvm::Constant *value = llvm::ConstantInt::get(
            type.type,
            enumField->useSigned ? enumField->sValue : enumField->uValue);
        irValue.value = value;
        irValue.type = type;
    }
    else if (maExpr->accessType == MemberAccessExpression::AccessType::PACKAGE)
    {
        auto pgkType = dynamic_cast<PackageDataType *>(
            semanticAnalyzer.exprToDataTypeMap[maExpr->GetLeftExpression()]);
        auto temp = semanticAnalyzer.currentScope;
        semanticAnalyzer.currentScope =
            semanticAnalyzer.pkgToScopeMap[pgkType->pkg];

        irValue = GenExpressionIR(maExpr->GetRightExpression());

        semanticAnalyzer.currentScope = temp;
    }

    return irValue;
}
IRValue CodeGen::GenPrimaryExpressionIR(PrimaryExpression *primaryExpr,
                                        bool isWrite)
{
    IRValue irValue;
    DataType *primaryDataType = semanticAnalyzer.exprToDataTypeMap[primaryExpr];
    irValue.type = ZtoonTypeToLLVMType(primaryDataType);
    switch (primaryExpr->GetPrimary()->GetType())
    {
    case TokenType::INTEGER_LITERAL:
    {
        auto integerType = semanticAnalyzer.exprToDataTypeMap[primaryExpr];
        uint64_t value = 0;
        if (dynamic_cast<TokenLiteral<int32_t> const *>(
                primaryExpr->GetPrimary()))
        {
            auto const *literal = dynamic_cast<TokenLiteral<int32_t> const *>(
                primaryExpr->GetPrimary());
            value = literal->GetValue();
            irValue.value = llvm::ConstantInt::get(irValue.type.type, value);
            break;
        }
        switch (integerType->GetType())
        {
        case DataType::Type::I32:
        {
            auto const *literal = dynamic_cast<TokenLiteral<int32_t> const *>(
                primaryExpr->GetPrimary());
            value = literal->GetValue();
        }
        break;
        case DataType::Type::U32:
        {
            auto const *literal = dynamic_cast<TokenLiteral<uint32_t> const *>(
                primaryExpr->GetPrimary());
            value = literal->GetValue();
        }
        break;
        case DataType::Type::U64:
        {
            auto const *literal = dynamic_cast<TokenLiteral<uint64_t> const *>(
                primaryExpr->GetPrimary());
            value = literal->GetValue();
        }
        break;
        default:
        {
            ReportError(std::format("Wrong integer literal type '{}'",
                                    primaryExpr->GetCodeErrString().str),
                        primaryExpr->GetCodeErrString());
        }
        break;
        }

        irValue.value = llvm::ConstantInt::get(irValue.type.type, value);
        break;
    }
    case TokenType::FLOAT_LITERAL:
    {
        TokenLiteral<float> const *literal =
            dynamic_cast<TokenLiteral<float> const *>(
                primaryExpr->GetPrimary());
        irValue.value =
            llvm::ConstantFP::get(irValue.type.type, literal->GetValue());
        break;
    }

    case TokenType::CHARACTER_LITERAL:
    {
        TokenLiteral<int8_t> const *literal =
            dynamic_cast<TokenLiteral<int8_t> const *>(
                primaryExpr->GetPrimary());
        irValue.value =
            llvm::ConstantInt::get(irValue.type.type, literal->GetValue());
        break;
    }
    case TokenType::TRUE:
    {
        irValue.value = llvm::ConstantInt::getTrue(*ctx);
        break;
    }
    case TokenType::FALSE:
    {
        irValue.value = llvm::ConstantInt::getFalse(*ctx);
        break;
    }
    case TokenType::NULL_PTR:
    {
        irValue.value = llvm::Constant::getNullValue(irValue.type.type);
        break;
    };
    case TokenType::IDENTIFIER:
    {
        // are we in global scope?
        llvm::BasicBlock *block = irBuilder->GetInsertBlock();

        IRSymbol *symbol = GetIRSymbol(primaryExpr->GetPrimary()->GetLexeme());
        IRReadonlySymbol *irReadonlySymbol =
            dynamic_cast<IRReadonlySymbol *>(symbol);
        IRVariable *var = dynamic_cast<IRVariable *>(symbol);
        IRFunction *fn = dynamic_cast<IRFunction *>(symbol);
        if (var)
        {
            if (isWrite || !block)
            {
                irValue.type.type = symbol->GetType();
                irValue.value = symbol->GetValue();
            }
            else
            {
                irValue.value = irBuilder->CreateLoad(
                    symbol->GetType(), symbol->GetValue(), "load_var_value");
                auto ptrType = dynamic_cast<PointerDataType *>(primaryDataType);
                if (ptrType)
                {
                    irValue.type =
                        ZtoonTypeToLLVMType(ptrType->PointedToDatatype());
                }
                else
                {
                    irValue.type = ZtoonTypeToLLVMType(primaryDataType);
                }
            }
        }
        else if (fn)
        {
            // fn type
            irValue.type.type = symbol->GetType()->getPointerTo();
            irValue.value = symbol->GetValue();
        }
        else if (irReadonlySymbol)
        {
            irValue.value = irReadonlySymbol->value;
            irValue.type = irReadonlySymbol->irType;
        }
        else
        {
            ReportError(std::format("Unkown symbol"),
                        primaryExpr->GetCodeErrString());
        }
    }
    break;
    case TokenType::STRING_LITERAL:
    {
        TokenLiteral<std::string> const *literal =
            dynamic_cast<TokenLiteral<std::string> const *>(
                primaryExpr->GetPrimary());
        auto str_const =
            llvm::ConstantDataArray::getString(*ctx, literal->GetValue());
        llvm::GlobalVariable *varInReadOnlySection = new llvm::GlobalVariable(
            *module, str_const->getType(), true,
            llvm::GlobalValue::PrivateLinkage, str_const, "str_ro_section");
        varInReadOnlySection->setSection(".rodata");
        varInReadOnlySection->setAlignment(llvm::Align(1));
        irValue.value = irBuilder->CreateInBoundsGEP(
            varInReadOnlySection->getType(), varInReadOnlySection,
            {irBuilder->getInt32(0), irBuilder->getInt32(0)});
        irValue.type.type = irValue.value->getType();
    }
    break;
    default:
    {
        ReportError(std::format("This primary type '{}'  is not supported",
                                primaryExpr->GetPrimary()->GetLexeme()),
                    primaryExpr->GetCodeErrString());
        break;
    }
    }
    return irValue;
}
IRValue CodeGen::GenExpressionIR(Expression *expression, bool isWrite)
{
    // debugInfo->SetDebugLoc(expression->GetFirstToken());
    IRValue irValue = {};
    if (dynamic_cast<FnExpression *>(expression))
    {
        auto *fnExpr = dynamic_cast<FnExpression *>(expression);
        irValue = GenFnExpressionIR(fnExpr, isWrite);
    }
    else if (dynamic_cast<FnCallExpression *>(expression))
    {
        auto *fnCallExpr = dynamic_cast<FnCallExpression *>(expression);
        irValue = GenFnCallExpressionIR(fnCallExpr, isWrite);
    }
    else if (dynamic_cast<MemberAccessExpression *>(expression))
    {
        MemberAccessExpression *maExpr =
            dynamic_cast<MemberAccessExpression *>(expression);
        irValue = GenMemberAccessExpressionIR(maExpr, isWrite);
    }
    else if (dynamic_cast<SubscriptExpression *>(expression))
    {
        SubscriptExpression *subExpr =
            dynamic_cast<SubscriptExpression *>(expression);
        irValue = GenSubScriptExpressionIR(subExpr, isWrite);
    }
    else if (dynamic_cast<TernaryExpression *>(expression))
    {
        TernaryExpression *ternaryExpr =
            dynamic_cast<TernaryExpression *>(expression);
        irValue = GenTernaryExpressionIR(ternaryExpr, isWrite);
    }
    else if (dynamic_cast<BinaryExpression *>(expression))
    {
        BinaryExpression *binaryExpression =
            dynamic_cast<BinaryExpression *>(expression);
        irValue = GenBinaryExpressionIR(binaryExpression, isWrite);
    }
    else if (dynamic_cast<UnaryExpression *>(expression))
    {
        UnaryExpression *unaryExpression =
            dynamic_cast<UnaryExpression *>(expression);
        irValue = GenUnaryExpressionIR(unaryExpression, isWrite);
    }
    else if (dynamic_cast<GroupingExpression *>(expression))
    {
        GroupingExpression *groupingExpression =
            dynamic_cast<GroupingExpression *>(expression);
        irValue = GenGroupingExpressionIR(groupingExpression, isWrite);
    }
    else if (dynamic_cast<CastExpression *>(expression))
    {
        CastExpression *castExpression =
            dynamic_cast<CastExpression *>(expression);
        irValue = GenCastExpressionIR(castExpression, isWrite);
    }
    else if (dynamic_cast<PrimaryExpression *>(expression))
    {
        PrimaryExpression *primaryExpression =
            dynamic_cast<PrimaryExpression *>(expression);
        irValue = GenPrimaryExpressionIR(primaryExpression, isWrite);
    }
    else
    {
        ReportError("This expression is not supported.",
                    expression->GetCodeErrString());
    };

    return irValue;
}
