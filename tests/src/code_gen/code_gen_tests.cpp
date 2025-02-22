#include "parser/parser.h"
#include "semantic_analyzer/semantic_analyzer.h"
#include "ztest.h"

#include "code_gen/code_gen.h"

TEST(CodeGen)
{
    std::string source =
        R"(
    a: u32 = 3;
     a = a + 3 - 5.0 as u32;    
    )";
    Lexer lexer;
    lexer.Tokenize(source, "source.ztoon");
    Parser parser(lexer.GetTokens());
    auto stmts = parser.Parse();
    SemanticAnalyzer sa(stmts);
    sa.Analize();
    parser.PrettyPrintAST();

    CodeGen codeGen(sa);
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(codeGen.GetIRBuilder()->getInt32Ty(), false);

    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", codeGen.GetModule());

    llvm::BasicBlock *entryBlock =
        llvm::BasicBlock::Create(codeGen.GetCTX(), "entry", mainFunc);

    codeGen.GetIRBuilder()->SetInsertPoint(entryBlock);
    // code gen here for now.
    codeGen.GenIR();
    IRVariable *var = codeGen.GetIRVariable("a");
    llvm::Value *v =
        codeGen.GetIRBuilder()->CreateLoad(var->irType.type, var->aInsta);
    codeGen.GetIRBuilder()->CreateRet(v);
    if (llvm::verifyModule(*codeGen.GetModule(), &llvm::errs()))
    {
        llvm::errs() << "Error\n";
    }

    codeGen.GetModule()->print(llvm::outs(), nullptr);
}
