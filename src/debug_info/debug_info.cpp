#include "debug_info.h"
#include "parser/parser.h"
#include "semantic_analyzer/semantic_analyzer.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Metadata.h"
#include "llvm/Support/Alignment.h"
#include "llvm/Support/Casting.h"
#include <cassert>
#include <memory>

llvm::DIType *DebugInfo::ZtoonTypeToDIType(DataType *type)
{
    llvm::DIType *diType = nullptr;
    switch (type->GetType())
    {

    case DataType::Type::I8:
    {
        diType = diBuilder->createBasicType("i8", 8,
                                            type->IsSigned()
                                                ? llvm::dwarf::DW_ATE_signed
                                                : llvm::dwarf::DW_ATE_unsigned);
        break;
    }
    case DataType::Type::I16:
    {
        diType = diBuilder->createBasicType("i16", 16,
                                            type->IsSigned()
                                                ? llvm::dwarf::DW_ATE_signed
                                                : llvm::dwarf::DW_ATE_unsigned);
        break;
    }
    case DataType::Type::I32:
    {
        diType = diBuilder->createBasicType("i32", 32,
                                            type->IsSigned()
                                                ? llvm::dwarf::DW_ATE_signed
                                                : llvm::dwarf::DW_ATE_unsigned);
        break;
    }
    case DataType::Type::I64:
    {
        diType = diBuilder->createBasicType("i64", 64,
                                            type->IsSigned()
                                                ? llvm::dwarf::DW_ATE_signed
                                                : llvm::dwarf::DW_ATE_unsigned);
        break;
    }
    case DataType::Type::U8:
    {
        diType = diBuilder->createBasicType("u8", 8,
                                            type->IsSigned()
                                                ? llvm::dwarf::DW_ATE_signed
                                                : llvm::dwarf::DW_ATE_unsigned);
        break;
    }
    case DataType::Type::U16:
    {
        diType = diBuilder->createBasicType("u16", 16,
                                            type->IsSigned()
                                                ? llvm::dwarf::DW_ATE_signed
                                                : llvm::dwarf::DW_ATE_unsigned);
        break;
    }
    case DataType::Type::U32:
    {
        diType = diBuilder->createBasicType("u32", 32,
                                            type->IsSigned()
                                                ? llvm::dwarf::DW_ATE_signed
                                                : llvm::dwarf::DW_ATE_unsigned);
        break;
    }
    case DataType::Type::U64:
    {
        diType = diBuilder->createBasicType("u64", 64,
                                            type->IsSigned()
                                                ? llvm::dwarf::DW_ATE_signed
                                                : llvm::dwarf::DW_ATE_unsigned);
        break;
    }
    case DataType::Type::F32:
    {
        diType =
            diBuilder->createBasicType("f32", 32, llvm::dwarf::DW_ATE_float);
        break;
    }
    case DataType::Type::F64:
    {
        diType =
            diBuilder->createBasicType("f64", 64, llvm::dwarf::DW_ATE_float);
        break;
    }
    case DataType::Type::BOOL:
    {
        diType =
            diBuilder->createBasicType("bool", 8, llvm::dwarf::DW_ATE_boolean);
        break;
    }
    case DataType::Type::NOTYPE:
    {
        diType = diBuilder->createBasicType("notype", 0,
                                            llvm::dwarf::DW_ATE_unsigned);
        break;
    }
    case DataType::Type::STRUCT:
    {
        if (ztoonTypeToDITypeMap.contains(type->ToString()))
        {
            diType = ztoonTypeToDITypeMap[type->ToString()];
            break;
        }
        auto irStructType = llvm::dyn_cast<llvm::StructType>(
            codeGen->ZtoonTypeToLLVMType(type).type);
        std::vector<llvm::Metadata *> fields;
        auto ztoonStructType = dynamic_cast<StructDataType *>(type);
        uint32_t index = 0;
        uint64_t offset = 0;
        for (auto field : ztoonStructType->fields)
        {
            auto memeberType = ZtoonTypeToDIType(field);
            auto file = GetDIFile(ztoonStructType->structStmt->GetIDToken()
                                      ->GetFilepath()
                                      .generic_string());
            auto token = ztoonStructType->structStmt->GetFields()[index]
                             ->GetIdentifier();
            uint32_t lineNumber = token->GetLineNumber();
            std::string fName = token->GetLexeme();
            auto irFieldType = codeGen->ZtoonTypeToLLVMType(field).type;
            uint32_t fieldSize =
                codeGen->moduleDataLayout->getTypeSizeInBits(irFieldType);
            uint32_t fAlign =
                codeGen->moduleDataLayout->getABITypeAlign(irFieldType)
                    .value() *
                8;
            fields.push_back(diBuilder->createMemberType(
                file, fName, file, lineNumber, fieldSize, fAlign, offset,
                llvm::DINode::FlagZero, memeberType));
            offset += fieldSize;
            index++;
        }
        size_t sizeInBits =
            codeGen->moduleDataLayout->getTypeSizeInBits(irStructType);
        diType = diBuilder->createStructType(
            currentScope, ztoonStructType->GetFullName(),
            GetDIFile(ztoonStructType->structStmt->GetIDToken()
                          ->GetFilepath()
                          .generic_string()),
            ztoonStructType->structStmt->GetIDToken()->GetLineNumber(),
            sizeInBits,
            codeGen->moduleDataLayout->getABITypeAlign(irStructType).value() *
                8,
            llvm::DINode::DIFlags::FlagLittleEndian, nullptr,
            diBuilder->getOrCreateArray(fields));
    }
    break;
    case DataType::Type::ENUM:
    {
        auto enumType = dynamic_cast<EnumDataType *>(type);
        auto derivedType = enumType->datatype;
        diType = ZtoonTypeToDIType(derivedType);
    }
    break;
    case DataType::Type::UNION:
    {
        auto unionType = dynamic_cast<UnionDataType *>(type);
        diType = ZtoonTypeToDIType(unionType->largestDatatype);
        break;
    }
    case DataType::Type::POINTER:
    {
        auto ptrType = dynamic_cast<PointerDataType *>(type);
        llvm::DIType *baseType =
            ZtoonTypeToDIType(ptrType->PointedToDatatype());
        diType = diBuilder->createPointerType(
            baseType, codeGen->GetPtrBitWidth(), 0, llvm::dwarf::DW_LANG_C,
            ptrType->ToString());
        break;
    }
    case DataType::Type::ARRAY:
    {
        auto arrayType = dynamic_cast<ArrayDataType *>(type);
        auto irArrayType = codeGen->ZtoonTypeToLLVMType(type);
        size_t size =
            codeGen->moduleDataLayout->getTypeSizeInBits(irArrayType.type);
        llvm::Align align =
            codeGen->moduleDataLayout->getABITypeAlign(irArrayType.type);
        uint32_t alignment = align.value() * 8;
        diType = diBuilder->createArrayType(
            size, alignment, ZtoonTypeToDIType(arrayType->dataType),
            diBuilder->getOrCreateArray(
                diBuilder->getOrCreateSubrange(0, (int64_t)arrayType->size)));
    }
    break;
    case DataType::Type::FN:
    {
        FnDataType *fnType = dynamic_cast<FnDataType *>(type);
        std::vector<llvm::Metadata *> types;
        llvm::DIType *retType = ZtoonTypeToDIType(fnType->GetReturnDataType());
        types.push_back(retType);
        for (auto param : fnType->GetParameters())
        {
            types.push_back(ZtoonTypeToDIType(param));
        }
        diType = diBuilder->createSubroutineType(
            diBuilder->getOrCreateTypeArray(types));
    }
    break;
    default:
    {
        break;
    }
    }

    return diType;
}

DebugInfo::DebugInfo(CodeGen *codeGen)
{
    this->codeGen = codeGen;
    diBuilder = std::make_unique<llvm::DIBuilder>(*codeGen->module);

    auto zPath = codeGen->project->relativePathToWorkSpace / "ztoon.yaml";

    auto file = diBuilder->createFile(zPath.filename().string(),
                                      zPath.parent_path().generic_string());
    auto cu =
        diBuilder->createCompileUnit(llvm::dwarf::DW_LANG_C, file, "ztoon",
                                     !codeGen->project->debugBuild, "", 0);
    currentScope = file;
}
DebugInfo::~DebugInfo() {}

void DebugInfo::Finalize() { diBuilder->finalize(); }

llvm::DIFile *DebugInfo::GetDIFile(std::string filepath)
{
    if (filepathToDIFile.contains(filepath))
    {
        return filepathToDIFile[filepath];
    }
    std::filesystem::path path = filepath;
    auto file = diBuilder->createFile(path.filename().string(),
                                      path.parent_path().generic_string());
    filepathToDIFile[filepath] = file;
    return file;
}
void DebugInfo::GenFnStatementDI(FnStatement *fnStmt, FnExpression *fnExpr,
                                 IRFunction *irFunc)
{
    auto filepath =
        fnStmt ? fnStmt->GetIdentifier()->GetFilepath().generic_string()
               : fnExpr->GetFirstToken()->GetFilepath().generic_string();
    uint32_t lineNumber = fnStmt ? fnStmt->GetIdentifier()->GetLineNumber()
                                 : fnExpr->GetFirstToken()->GetLineNumber();
    llvm::DIFile *file = GetDIFile(filepath);
    auto ztoonType =
        dynamic_cast<Function *>(
            codeGen->semanticAnalyzer.currentScope->GetSymbol(
                irFunc->GetName(), fnStmt ? fnStmt->GetCodeErrString()
                                          : fnExpr->GetCodeErrString()))
            ->GetFnDataTypeFromFnPTR();
    auto fnType =
        llvm::dyn_cast<llvm::DISubroutineType>(ZtoonTypeToDIType(ztoonType));
    bool flagZero = (fnStmt && !fnExpr && fnStmt->IsPrototype());

    llvm::DISubprogram *sp = diBuilder->createFunction(
        file, irFunc->GetName(), irFunc->GetFullName(), file, lineNumber,
        fnType, lineNumber, llvm::DINode::DIFlags::FlagZero,
        flagZero ? llvm::DISubprogram::SPFlagZero
                 : llvm::DISubprogram::DISPFlags::SPFlagDefinition);
    irFunc->fn->setSubprogram(sp);
}

void DebugInfo::GenVarDeclStatementDI(VarDeclStatement *varStmt,
                                      IRVariable *irVariable, bool isGlobal)
{

    auto ztoonType = codeGen->semanticAnalyzer.stmtToDataTypeMap[varStmt];
    llvm::DIType *diType = ZtoonTypeToDIType(ztoonType);
    llvm::DIFile *file =
        GetDIFile(varStmt->GetIdentifier()->GetFilepath().generic_string());
    std::string fullname = irVariable->GetName();
    std::string name = varStmt->GetIdentifier()->GetLexeme();
    uint32_t lineNumber = varStmt->GetIdentifier()->GetLineNumber();
    uint32_t colNumber = varStmt->GetIdentifier()->GetColNumber();
    if (isGlobal)
    {
        currentScope = file;
        auto digv = diBuilder->createGlobalVariableExpression(
            currentScope, name, fullname, file, lineNumber, diType, false);
        auto gv = llvm::dyn_cast<llvm::GlobalVariable>(irVariable->value);
        gv->addDebugInfo(digv);
    }
    else
    {
        currentScope =
            codeGen->irBuilder->GetInsertBlock()->getParent()->getSubprogram();
        assert(currentScope && "Scope is null!");
        assert(isa<llvm::DILocalScope>(currentScope) &&
               "Invalid scope for local variable!");
        auto div = diBuilder->createAutoVariable(currentScope, name, file,
                                                 lineNumber, diType, false);
        auto var = llvm::dyn_cast<llvm::AllocaInst>(irVariable->value);
        diBuilder->insertDeclare(var, div, diBuilder->createExpression(),
                                 llvm::DILocation::get(*codeGen->ctx,
                                                       lineNumber, colNumber,
                                                       currentScope),
                                 codeGen->irBuilder->GetInsertBlock());
    }
}
void DebugInfo::SetDebugLoc(Token const *token)
{

    auto bb = codeGen->irBuilder->GetInsertBlock();
    if (bb)
    {
        currentScope = bb->getParent()->getSubprogram();
    }
    else
    {
        return;
    }

    uint32_t lineNumber = token->GetLineNumber();
    uint32_t colNumber = token->GetColNumber();
    std::filesystem::path filepath = token->GetFilepath();

    llvm::DILocation *loc = llvm::DILocation::get(*codeGen->ctx, lineNumber,
                                                  colNumber, currentScope);
    codeGen->irBuilder->SetCurrentDebugLocation(loc);
}
