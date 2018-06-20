package org.cornutum.tcases.annotation.reader;

import org.cornutum.tcases.conditions.ContainsAll;
import org.cornutum.tcases.conditions.ContainsAny;
import org.cornutum.tcases.conditions.Not;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.beans.SamePropertyValuesAs.samePropertyValuesAs;
import static org.junit.Assert.assertThat;

public class ConditionReaderTest {

    private static final String[] EMPTY = new String[0];

    @Test
    public void getConditionWhen() {
        assertThat(ConditionReader.getCondition(EMPTY, EMPTY), nullValue());
        assertThat(ConditionReader.getCondition(of("foo"), EMPTY),
                samePropertyValuesAs(new ContainsAll("foo")));
        assertThat(ConditionReader.getCondition(of("foo", "bar"), EMPTY),
                samePropertyValuesAs(new ContainsAll("foo", "bar")));
        assertThat(((Not) ConditionReader.getCondition(EMPTY, of("foo"))).getConditions().next(),
                samePropertyValuesAs((new ContainsAny("foo"))));
        assertThat(((Not) ConditionReader.getCondition(EMPTY, of("foo", "bar"))).getConditions().next(),
                samePropertyValuesAs((new ContainsAny("foo", "bar"))));
    }

    private static final String[] of(String... values) {
        return values;
    }
}