
namespace Test01 {

    use Text/RegexEval.MatchEvaluator;

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
        
    @test
    def matchAll04(): Bool =
        let ans = region r1 {
            test04Body(r1, Text/RegexEval/MatchEvaluator.content())
        };
        ans == Ok("Hello world")
    
    /// We can abstract the body of an "evaluation" into a separate function provided 
    /// the region and the eval function operate on the same region.
    def test04Body(r1: Region[r], eval: MatchEvaluator[r, String, String]): Result[String, String] \ Read(r) =
        use Result.{flatMap}; 
        let* p1 = Text/Regex/Pattern.compile("[\\s\\S]*");
        Text/RegexEval/Regex.matchAll(r1, eval, p1, "Hello world")

    /// Thunk version escapes too...

    /// @test
    /// def matchAll05(): Bool =
    ///     use Result.{flatMap};
    ///     let makeEval = () -> Text/RegexEval/MatchEvaluator.content();
    ///     region r1 {
    ///         let step = () -> {                
    ///             let* p1 = Text/Regex/Pattern.compile("[\\s\\S]*");
    ///             Text/RegexEval/Regex.matchAll(r1, makeEval(), p1, "Hello world")
    ///         };
    ///         step() == Ok("Hello world")
    ///     }

}