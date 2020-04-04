//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.hamcrest.BaseCompositeMatcher;
import org.cornutum.hamcrest.Composites;
import java.util.Map;
import java.util.function.Function;

/**
 * A composite matcher for {@link ObjectValue} data objects.
 */
public class ObjectDataMatcher extends BaseCompositeMatcher<Map<String,DataValue<?>>>
  {
  /**
   * Creates a new ObjectDataMatcher instance.
   */
  public ObjectDataMatcher( Map<String,DataValue<?>> expected)
    {
    super( expected);

    expectThat( valueOf( "properties", this::getProperties).matches( Composites::containsMembers));
    expected.keySet().forEach( key -> expectThat( valueOf( key, new PropertyAccessor( key)).matches( DataValueMatcher::new)));
    }

  /**
   * Returns the set of object properties.
   */
  private Iterable<String> getProperties( Map<String,DataValue<?>> data)
    {
    return data.keySet();
    }

  /**
   * A {@link Function} that returns the value of a specific data property.
   */
  @SuppressWarnings("rawtypes")
public static class PropertyAccessor implements Function<Map<String,DataValue<?>>,DataValue>
    {
    /**
     * Creates a new PropertyAccessor instance.
     */
    public PropertyAccessor( String key)
      {
      key_ = key;
      }

    public DataValue apply( Map<String,DataValue<?>> data)
      {
      return data.get( key_);
      }
    
    private final String key_;
    }
  }
