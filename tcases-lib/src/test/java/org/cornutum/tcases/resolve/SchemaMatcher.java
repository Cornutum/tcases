//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.hamcrest.BaseCompositeMatcher;
import org.hamcrest.Matchers;

import java.util.List;
import static java.util.stream.Collectors.toList;

/**
 * A composite matcher for {@link Schema} objects.
 */
public class SchemaMatcher extends BaseCompositeMatcher<Schema>
  {
  /**
   * Creates a new SchemaMatcher instance.
   */
  public SchemaMatcher( Schema expected)
    {
    super( expected);

    expectThat( valueOf( "type", Schema::getType).matches( Matchers::equalTo));
    expectThat( valueOf( "constant", this::getConstant).matches( DataValueMatcher::new));
    expectThat( valueOf( "enum", this::getEnum).matches( containsMembersMatching( DataValueMatcher::new)));
    expectThat( valueOf( "format", Schema::getFormat).matches( Matchers::equalTo));
    expectThat( valueOf( "minimum", Schema::getMinimum).matches( Matchers::equalTo));
    expectThat( valueOf( "maximum", Schema::getMaximum).matches( Matchers::equalTo));
    expectThat( valueOf( "exclusiveMinimum", Schema::getExclusiveMinimum).matches( Matchers::equalTo));
    expectThat( valueOf( "exclusiveMaximum", Schema::getExclusiveMaximum).matches( Matchers::equalTo));
    expectThat( valueOf( "multipleOf", Schema::getMultipleOf).matches( Matchers::equalTo));
    expectThat( valueOf( "minLength", Schema::getMinLength).matches( Matchers::equalTo));
    expectThat( valueOf( "maxLength", Schema::getMaxLength).matches( Matchers::equalTo));
    expectThat( valueOf( "pattern", Schema::getPattern).matches( Matchers::equalTo));
    expectThat( valueOf( "minItems", Schema::getMinItems).matches( Matchers::equalTo));
    expectThat( valueOf( "maxItems", Schema::getMaxItems).matches( Matchers::equalTo));
    expectThat( valueOf( "uniqueItems", Schema::getUniqueItems).matches( Matchers::equalTo));
    expectThat( valueOf( "items", Schema::getItems).matches( SchemaMatcher::new));
    }

  @SuppressWarnings("rawtypes")
  private DataValue getConstant( Schema schema)
    {
    return schema.getConstant();
    }

  @SuppressWarnings("rawtypes")
  private List<DataValue> getEnum( Schema schema)
    {
    return
      schema.getEnum() == null
      ? null
      : schema.getEnum().stream().collect( toList());
    }
  }

