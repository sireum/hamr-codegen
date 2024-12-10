package org.sireum.hamr.codegen;

import org.sireum.*;

import java.lang.String;

public class CodeGenJavaFactory {

    public static <T> org.sireum.Option<T> sireumOption(T o) {
        if(o != null) {
            return Some$.MODULE$.apply(o);
        } else {
            return None$.MODULE$.apply();
        }
    }

    public static ST stringToST(String s) {
        return CodeGenFactory.stringToST(s);
    }

    public static <T> ST iszToST (IS<Z, T> elements, String sep) { return CodeGenFactory.iszToST(elements, sep); }
}
