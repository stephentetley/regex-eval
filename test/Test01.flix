
namespace Test01 {

    @test
    def matchAll01(): Bool = 
        use Result.{flatMap};
        let step = () -> {
            region r1 {
                let* p1 = Text/Regex/Pattern.compile("\\w+");
                Text/RegexEval/Regex.matchAll(r1, Text/RegexEval/MatchEvaluator.content(), p1, "aaaaa")
            }
        };
        step() == Ok("aaaaa")

    @test
    def matchAll02(): Bool = 
        use Result.{flatMap};
        let step = () -> {
            region r1 {
                let* p1 = Text/Regex/Pattern.compile("[\\s\\S]*");
                Text/RegexEval/Regex.matchAll(r1, Text/RegexEval/MatchEvaluator.content(), p1, "Hello world")
            }
        };
        step() == Ok("Hello world")

    @test
    def matchAll03(): Bool =
        use Result.{flatMap};
        region r1 {
            let step = () -> {
                let eval = Text/RegexEval/MatchEvaluator.content();
                let* p1 = Text/Regex/Pattern.compile("[\\s\\S]*");
                Text/RegexEval/Regex.matchAll(r1, eval, p1, "Hello world")
            };
            step() == Ok("Hello world")
        }
        

}