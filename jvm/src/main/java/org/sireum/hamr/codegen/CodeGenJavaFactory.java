package org.sireum.hamr.codegen;

import org.sireum.IS;
import org.sireum.Z;
import org.sireum.ST;

public class CodeGenJavaFactory {

    public static ST stringToST(String s) {
        return CodeGenFactory.stringToST(s);
    }

    public static <T> ST iszToST (IS<Z, T> elements, String sep) { return CodeGenFactory.iszToST(elements, sep); }
}
