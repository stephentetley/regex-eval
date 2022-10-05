/*
 * Copyright 2020 Stephen Tetley
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



namespace Text/RegexEval {

    use Text/Regex.{Matcher};

    ///
    /// Matcher is a monad (mutable-state, error).
    ///
    pub enum MatchEvaluator[r: Region, ka: Type, a: Type](MonadLib.Evaluator[r, ka, Matcher[r], a])


    instance Functor[MatchEvaluator[r, ka]] {
        pub def map(f: a -> b & ef, o: MatchEvaluator[r, ka, a]): MatchEvaluator[r, ka, b] & ef =
            Text/RegexEval/MatchEvaluator.map(f, o)
    }

    instance Applicative[MatchEvaluator[r, ka]] {
        pub def point(x: a): MatchEvaluator[r, ka, a] = 
            Text/RegexEval/MatchEvaluator.point(x)

        pub def ap(f: MatchEvaluator[r, ka, a -> b & ef], x: MatchEvaluator[r, ka, a]): MatchEvaluator[r, ka, b] & ef =
            Text/RegexEval/MatchEvaluator.ap(f, x)
    }

    instance Monad[MatchEvaluator[r, ka]] {
        pub def flatMap(f: a -> MatchEvaluator[r, ka, b] & ef, o: MatchEvaluator[r, ka, a]): MatchEvaluator[r, ka, b] & ef =
            Text/RegexEval/MatchEvaluator.flatMap(f, o)
    }

    instance MonadZero[MatchEvaluator[r, ka]] {
        pub def empty(): MatchEvaluator[r, ka, a] = Text/RegexEval/MatchEvaluator.empty()
    }

    instance MonadLib.MonadOr[MatchEvaluator[r, ka]] {
        pub def orelse(a: MatchEvaluator[r, ka, a], b: MatchEvaluator[r, ka, a]): MatchEvaluator[r, ka, a] =
            Text/RegexEval/MatchEvaluator.orelse(a, b)
    }

    instance MonadLib.MonadError[MatchEvaluator[r, ka]] { 
        pub def throwError(msg: String): MatchEvaluator[r, ka, a] =
            Text/RegexEval/MatchEvaluator.throwError(msg)

        pub def mapError(f: String -> String \ ef, ma: MatchEvaluator[r, ka, a]): MatchEvaluator[r, ka, a] \ ef = 
            Text/RegexEval/MatchEvaluator.mapError(f, ma)

        pub def catchError(ma: MatchEvaluator[r, ka, a], hdlr: String -> MatchEvaluator[r, ka, a] \ ef): MatchEvaluator[r, ka, a] \ ef = 
            Text/RegexEval/MatchEvaluator.catchError(ma, hdlr)
    }  

}

namespace Text/RegexEval/MatchEvaluator {

    use Applicative.{liftA2};

    use Text/RegexEval.MatchEvaluator;
    use Text/RegexEval.MatchEvaluator.{MatchEvaluator};

    use Text/Regex.Pattern;
    use Text/Regex.Pattern.{Pattern};
    use Text/Regex.Matcher;
    
    pub def getMatchEvaluator(x: MatchEvaluator[r, ka, a]): MonadLib.Evaluator[r, ka, Matcher[r], a] = 
        let MatchEvaluator(x1) = x;
        x1


    pub def runMatchEvaluator(r: Region[r], 
                                ma: MatchEvaluator[r, a, a], 
                                st: Matcher[r]): Result[a, String] & r =
        MonadLib/Evaluator.runEvaluator(r, getMatchEvaluator(ma), st)


    pub def point(x: a): MatchEvaluator[r, ka, a] = MatchEvaluator(MonadLib/Evaluator.point(x))



    pub def map(f: a -> b & ef, x: MatchEvaluator[r, ka, a]): MatchEvaluator[r, ka, b] & ef =
        MatchEvaluator(MonadLib/Evaluator.map(f, getMatchEvaluator(x)))

    pub def ap(mf: MatchEvaluator[r, ka, a -> b & ef], ma: MatchEvaluator[r, ka, a]): MatchEvaluator[r, ka, b] & ef =
        MatchEvaluator(MonadLib/Evaluator.ap(getMatchEvaluator(mf), getMatchEvaluator(ma)))

    
    pub def flatMap(k: a -> MatchEvaluator[r, ka, b] & ef, x: MatchEvaluator[r, ka, a]): MatchEvaluator[r, ka, b] & ef =
        MatchEvaluator(MonadLib/Evaluator.flatMap(k >> getMatchEvaluator, getMatchEvaluator(x)))

    pub def throwError(msg: String): MatchEvaluator[r, ka, a] = 
        MatchEvaluator(MonadLib/Evaluator.throwError(msg))
  

    pub def mapError(f: String -> String \ ef, ma: MatchEvaluator[r, ka, a]): MatchEvaluator[r, ka, a] \ ef = 
        MatchEvaluator(MonadLib/Evaluator.mapError(f, getMatchEvaluator(ma)))

    pub def catchError(ma: MatchEvaluator[r, ka, a], hdlr: String -> MatchEvaluator[r, ka, a] \ ef): MatchEvaluator[r, ka, a] \ ef = 
        MatchEvaluator(MonadLib/Evaluator.catchError(getMatchEvaluator(ma), hdlr >> getMatchEvaluator))

    // MatchEvaluator can be MonadOr because it does not "step".
    // It is only runs on the current match.

    pub def empty(): MatchEvaluator[r, ka, a] = throwError("empty")

    pub def orelse(ma: MatchEvaluator[r, ka, a], mb: MatchEvaluator[r, ka, a]): MatchEvaluator[r, ka, a] =
        MatchEvaluator(MonadLib/Evaluator.orelse(getMatchEvaluator(ma), getMatchEvaluator(mb)))


    def liftGetter(f: Matcher[r] -> a & ef): MatchEvaluator[r, ka, a] & ef =
        MatchEvaluator(MonadLib/Evaluator.liftGetter(f))

    def liftMatcherApp(f: Matcher[r] -> Result[a, String] & ef): MatchEvaluator[r, ka, a] & ef =
        use Monad.{>>=};
        liftGetter(f) >>= ans -> match ans {
            case Ok(a)    => point(a)
            case Err(err) => throwError(err)
        }



    pub def liftOption(err: String, x: Option[a]): MatchEvaluator[r, ka, a] = match x {
        case Some(x1) => point(x1)
        case None     => throwError(err)
    }


    pub def liftResult(x: Result[a, String]): MatchEvaluator[r, ka, a] = match x {
        case Ok(x1)     => point(x1)
        case Err(err)   => throwError(err)
    }


    /// pub def optional(mx: MatchEvaluator[r, ka, a]): MatchEvaluator[r, ka, Option[a]] =
    ///     MatchEvaluator(s -> match apply1(mx, s) {
    ///         case Ok(a)  => Ok(Some(a))
    ///         case Err(_) => Ok(None)
    ///     })

    /// pub def withDefault(d: a, mx: MatchEvaluator[r, ka, a]): MatchEvaluator[r, ka, a] =
    ///     MatchEvaluator(s -> match apply1(mx, s) {
    ///         case Ok(a)  => Ok(a)
    ///         case Err(_) => Ok(d)
    ///     })



    pub def content(): MatchEvaluator[r, ka, String] \ Read(r) =         
        liftMatcherApp(Text/Regex/Matcher.content)


    /// TODO - is this worthwhile?
    pub def transformContent(f: String -> a & ef): MatchEvaluator[r, ka, a] \ {ef, Read(r)} = map(f, content())


    pub def getStart(): MatchEvaluator[r, ka, Int32] \ Read(r) = 
        liftMatcherApp(Text/Regex/Matcher.start)

    pub def getEnd(): MatchEvaluator[r, ka, Int32] \ Read(r) =
        liftMatcherApp(Text/Regex/Matcher.end)

    pub def getLength(): MatchEvaluator[r, ka, Int32] \ Read(r) =
        liftA2((x,y) -> y - x, getStart(), getEnd()) |> MonadLib/MonadError.swapError("getLength")

    ///
    ///
    pub def getBounds(): MatchEvaluator[r, ka, {start :: Int32, end :: Int32}] \ Read(r) =
        liftA2((x,y) -> {start = x, end = y}, getStart(), getEnd()) |> MonadLib/MonadError.swapError("getBounds")


    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)` uses 1-indexing.
    ///
    pub def getCaptureAt(ix: Int32): MatchEvaluator[r, ka, String] \ Read(r) =
        liftMatcherApp(Text/Regex/Matcher.groupAt(ix))

    pub def getCaptureNamed(label: String): MatchEvaluator[r, ka, String] \ Read(r) =
        liftMatcherApp(Text/Regex/Matcher.groupNamed(label))


/*
    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)` uses 1-indexing.
    ///
    pub def getCaptureStartAt(ix: Int32): MatchEvaluator[Int32] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.startWithIndex(x, ix+1))
            |> swapError("getCaptureStartAt(${ix})")

    pub def getCaptureStartNamed(label: String): MatchEvaluator[Int32] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.startWithLabel(x, label))
            |> swapError("getCaptureStartNamed('${label}')")

    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)`
    /// uses 1-indexing.
    ///
    pub def getCaptureEndAt(ix: Int32): MatchEvaluator[Int32] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.endWithIndex(x, ix+1))
            |> swapError("getCaptureEndAt(${ix})")

    pub def getCaptureEndNamed(label: String): MatchEvaluator[Int32] =
        liftPrimResult(x -> Text/Regex/Internal/Prim.endWithLabel(x, label))
            |> swapError("getCaptureEndNamed('${label}')")

    ///
    /// Note - this function uses 0-indexing, the equivalent Java method `group(int group)`
    /// uses 1-indexing.
    ///
    pub def getCaptureRangeAt(ix: Int32): MatchEvaluator[{start :: Int32, end :: Int32}] =
        liftA2((x,y) -> {start = x, end = y}, getCaptureStartAt(ix), getCaptureEndAt(ix))
            |> swapError("getCaptureRangeAt(${ix})")

    pub def getCaptureRangeNamed(label: String): MatchEvaluator[{start :: Int32, end :: Int32}] =
        liftA2((x, y) -> {start = x, end = y}, getCaptureStartNamed(label), getCaptureEndNamed(label))
            |> swapError("getCaptureRangeNamed('${label}')")


*/

    pub def getInt8At(ix: Int32): MatchEvaluator[r, ka, Int8] \ Read(r) =
        let* str = getCaptureAt(ix);
        liftOption("getInt8At(${ix})", FromString.fromString(str): Option[Int8])

    pub def getInt8Named(label: String): MatchEvaluator[r, ka, Int8] \ Read(r) =
        let* str = getCaptureNamed(label);
        liftOption("getInt8Named('${label}')", FromString.fromString(str): Option[Int8])

    pub def getInt16At(ix: Int32): MatchEvaluator[r, ka, Int16] \ Read(r) =
        let* str = getCaptureAt(ix);
        liftOption("getInt16At(${ix})", FromString.fromString(str): Option[Int16])

    pub def getInt16Named(label: String): MatchEvaluator[r, ka, Int16] \ Read(r) =
        let* str = getCaptureNamed(label);
        liftOption("getInt16Named('${label}')", FromString.fromString(str): Option[Int16])

    pub def getInt32At(ix: Int32): MatchEvaluator[r, ka, Int32] \ Read(r) =
        let* str = getCaptureAt(ix);
        liftOption("getInt32At(${ix})", FromString.fromString(str): Option[Int32])

    pub def getInt32Named(label: String): MatchEvaluator[r, ka, Int32] \ Read(r) =
        let* str = getCaptureNamed(label);
        liftOption("getInt32Named('${label}')", FromString.fromString(str): Option[Int32])

    pub def getInt64At(ix: Int32): MatchEvaluator[r, ka, Int64] \ Read(r) =
        let* str = getCaptureAt(ix);
        liftOption("getInt64At(${ix})", FromString.fromString(str): Option[Int64])

    pub def getInt64Named(label: String): MatchEvaluator[r, ka, Int64] \ Read(r) =
        let* str = getCaptureNamed(label);
        liftOption("getInt64Named('${label}')", FromString.fromString(str): Option[Int64])

    pub def getBigIntAt(ix: Int32): MatchEvaluator[r, ka, BigInt] \ Read(r) =
        let* str = getCaptureAt(ix);
        liftOption("getBigIntAt(${ix})", FromString.fromString(str): Option[BigInt])

    pub def getBigIntNamed(label: String): MatchEvaluator[r, ka, BigInt] \ Read(r) =
        let* str = getCaptureNamed(label);
        liftOption("getBigIntNamed('${label}')", FromString.fromString(str): Option[BigInt])

    pub def getFloat32At(ix: Int32): MatchEvaluator[r, ka, Float32] \ Read(r) =
        let* str = getCaptureAt(ix);
        liftOption("getFloat32At(${ix})", FromString.fromString(str): Option[Float32])

    pub def getFloat32Named(label: String): MatchEvaluator[r, ka, Float32] \ Read(r) =
        let* str = getCaptureNamed(label);
        liftOption("getFloat32Named('${label}')", FromString.fromString(str): Option[Float32])

    pub def getFloat64At(ix: Int32): MatchEvaluator[r, ka, Float64] \ Read(r) =
        let* str = getCaptureAt(ix);
        liftOption("getFloat64At(${ix})", FromString.fromString(str): Option[Float64])

    pub def getFloat64Named(label: String): MatchEvaluator[r, ka, Float64] \ Read(r) =
        let* str = getCaptureNamed(label);
        liftOption("getFloat64Named('${label}')", FromString.fromString(str): Option[Float64])

}
