package org.cornutum.tcases.annotation.parser;

import org.cornutum.tcases.annotation.Value;
import org.cornutum.tcases.annotation.Var;
import org.junit.Test;

import static org.cornutum.tcases.annotation.parser.VarValueDefReader.getVarValueDefs;
import static org.hamcrest.Matchers.hasSize;
import static org.junit.Assert.assertThat;

public class VarValueDefReaderTest {

    @Test
    public void createVarValueDef() throws Exception {
        // VarValueDefReader.createVarValueDef();
    }

    @Test
    public void testGetVarValueDefs() throws Exception {
        Class<BooleanFieldSamples> fieldSamplesClass = BooleanFieldSamples.class;
        assertThat(getVarValueDefs(fieldSamplesClass.getField("aBoolean")),
                hasSize(2));
        assertThat(getVarValueDefs(fieldSamplesClass.getField("aBooleanWithVar")),
                hasSize(2));
        assertThat(getVarValueDefs(fieldSamplesClass.getField("aBooleanWithVar1Value")),
                hasSize(2));
        // TODO more tests
    }

    private static class BooleanFieldSamples {

        public Boolean aBoolean;

        @Var
        public Boolean aBooleanWithVar;

        @Var(values = @Value("true"))
        public Boolean aBooleanWithVar1Value;

        @Var(values = {@Value("true"), @Value("false")})
        public Boolean aBooleanWithVar2Value;
    }

    private static class EnumFieldSamples {

        public Enum0Sample enum0Field;
        public Enum1Sample enum1Field;
        public Enum3Sample enum3Field;

        public enum Enum0Sample {
        }

        public enum Enum1Sample {
        }

        public enum Enum3Sample {
            A1, A2, A3
        }
    }
}