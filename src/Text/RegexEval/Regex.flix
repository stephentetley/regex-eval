/*
 * Copyright 2022 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace Text/RegexEval/Regex {


    use Text/Regex.Pattern;
    use Text/Regex.Matcher;

    use Text/RegexEval.MatchEvaluator;
    use Text/RegexEval.MatchEvaluator.{MatchEvaluator};




    // # Simple extractors

    /// Whole input matches...
    ///
    pub def matchAll(r: Region[r], eval: MatchEvaluator[r, a, a], p: Pattern, s: String): Result[a, String] \ Read(r) =
        use Text/Regex/Matcher.{newMatcher, matches};
        let m = newMatcher(r, p, s);
        if (matches(m)) 
            Text/RegexEval/MatchEvaluator.runMatchEvaluator(r, eval, m)
        else
            Err("matchAll: mo match")

    /// No match is None rather than Err.
    pub def matchAllOptional(r: Region[r], eval: MatchEvaluator[r, a, a], p: Pattern, s: String): Result[Option[a], String] \ Read(r) =
        use Text/Regex/Matcher.{newMatcher, matches};
        let m = newMatcher(r, p, s);
        if (matches(m)) 
            Text/RegexEval/MatchEvaluator.runMatchEvaluator(r, eval, m) |> Result.map(Some)
        else
            Ok(None)


    ///
    /// Returns the first result matched by the pattern `p` on the input `s`.
    ///
    pub def firstSubmatch(r: Region[r], eval: MatchEvaluator[r, a, a], p: Pattern, s: String): Result[a, String] \ Read(r) =
        use Text/Regex/Matcher.{newMatcher, find!, matchResult};
        let m = newMatcher(r, p, s);
        if (find!(m))
            Text/RegexEval/MatchEvaluator.runMatchEvaluator(r, eval, m)
        else
            Err("firstSubmatch: mo match")

    ///
    /// Returns the first result matched by the pattern `p` on the input `s`.
    ///
    /// No match is None rather than Err.
    ///
    pub def firstSubmatchOptional(r: Region[r], eval: MatchEvaluator[r, a, a], p: Pattern, s: String): Result[Option[a], String] \ Read(r) =
        use Text/Regex/Matcher.{newMatcher, find!, matchResult};
        let m = newMatcher(r, p, s);
        if (find!(m))
            Text/RegexEval/MatchEvaluator.runMatchEvaluator(r, eval, m) |> Result.map(Some)
        else
            Ok(None)


    ///
    /// Returns the last result matched by the pattern `p` on the input `s`.
    ///
    pub def lastSubmatch(r: Region[r], eval: MatchEvaluator[r, a, a], p: Pattern, s: String): Result[a, String] \ Read(r) =
        use Text/Regex/Matcher.{newMatcher, findLast!, matchResult};
        let m = newMatcher(r, p, s);
        if (findLast!(m))
            Text/RegexEval/MatchEvaluator.runMatchEvaluator(r, eval, m)
        else
            Err("lastSubmatch: mo match")


    ///
    /// Returns the last result matched by the pattern `p` on the input `s`.
    ///
    pub def lastSubmatchOptional(r: Region[r], eval: MatchEvaluator[r, a, a], p: Pattern, s: String): Result[Option[a], String] \ Read(r) =
        use Text/Regex/Matcher.{newMatcher, findLast!, matchResult};
        let m = newMatcher(r, p, s);
        if (findLast!(m))
            Text/RegexEval/MatchEvaluator.runMatchEvaluator(r, eval, m) |> Result.map(Some)
        else
            Ok(None)


    ///
    /// Returns the start substring of `s` matched by the pattern `p`.
    ///
    pub def matchPrefix(r: Region[r], eval: MatchEvaluator[r, a, a], p: Pattern, s: String): Result[a, String] \ Read(r) =
        use Text/Regex/Matcher.{newMatcher, lookingAt, matchResult};
        let m = newMatcher(r, p, s);
        if (lookingAt(m)) 
            Text/RegexEval/MatchEvaluator.runMatchEvaluator(r, eval, m)
        else
            Err("matchPrefix: no match")
        

    ///
    /// Returns the start substring of `s` matched by the pattern `p`.
    ///
    pub def matchPrefixOptional(r: Region[r], eval: MatchEvaluator[r, a, a], p: Pattern, s: String): Result[Option[a], String] \ Read(r) =
        use Text/Regex/Matcher.{newMatcher, lookingAt, matchResult};
        let m = newMatcher(r, p, s);
        if (lookingAt(m)) 
            Text/RegexEval/MatchEvaluator.runMatchEvaluator(r, eval, m) |> Result.map(Some)
        else
            Ok(None)

    ///
    ///
    pub def boundedMatchAll(r: Region[r], 
                            eval: MatchEvaluator[r, a, a], 
                            start: {start = Int32}, 
                            end: {end = Int32}, 
                            p: Pattern, 
                            s: String): Result[a, String] \ Read(r) =
        use Text/Regex/Matcher.{newMatcher, setBounds!, matches, matchResult};
        let m = newMatcher(r, p, s);
        match setBounds!(start, end, m) {
            case Err(msg) => Err(msg)
            case Ok()     =>
                if (matches(m)) 
                    Text/RegexEval/MatchEvaluator.runMatchEvaluator(r, eval, m)
                else
                    Err("boundedMatchAll: no match")
        }

    pub def boundedFirstSubmatch(r: Region[r], 
                                    eval: MatchEvaluator[r, a, a], 
                                    start: {start = Int32}, 
                                    end: {end = Int32}, 
                                    p: Pattern, 
                                    s: String): Result[a, String] \ Read(r) =
        use Text/Regex/Matcher.{newMatcher, setBounds!, find!, matchResult};
        let m = newMatcher(r, p, s);
        match setBounds!(start, end, m) {
            case Err(msg) => Err(msg)
            case Ok()     =>
                if (find!(m)) 
                    Text/RegexEval/MatchEvaluator.runMatchEvaluator(r, eval, m)
                else
                    Err("boundedFirstSubmatch: no match")
        }

    ///
    /// Returns string `s` with every match of the pattern `from` replaced by the string `to`.
    ///
    pub def replace(r: Region[r], patt: {patt = Pattern}, to: {to = MatchEvaluator[r, String, String]}, s: String): Result[String, String] \ Read(r) =
        use Text/Regex/Matcher.{newMatcher};
        let m = newMatcher(r, patt.patt, s);
        region r1 { 
            let sb = new StringBuilder(r1);
            replaceHelper(r, sb, m, to.to) |> Result.map(_ -> StringBuilder.toString(sb))
        }


    def replaceHelper(r: Region[r], sb: StringBuilder[r1], m: Matcher[r], eval: MatchEvaluator[r, String, String]): Result[Unit, String] \ { Read(r), Write(r1) } =
        use Text/Regex/Matcher.{find!, appendReplacement!, appendTail!};
        if (find!(m))
            Text/RegexEval/MatchEvaluator.runMatchEvaluator(r, eval, m) |> Result.map(s -> appendReplacement!(s, sb, m))
        else { 
            appendTail!(sb, m); 
            Ok()
        }

}
