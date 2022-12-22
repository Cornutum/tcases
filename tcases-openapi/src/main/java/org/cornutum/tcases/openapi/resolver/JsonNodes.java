//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.tcases.resolve.DataValues.*;
import static org.cornutum.tcases.util.CollectionUtils.*;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.LinkedHashMap;

import org.cornutum.tcases.resolve.ArrayValue;
import org.cornutum.tcases.resolve.DataValue;
import org.cornutum.tcases.resolve.ObjectValue;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
  
/**
 * Defines methods to decode JsonNode objects.
 */
public final class JsonNodes
  {
  /**
   * Creates a new JsonNodes instance.
   */
  private JsonNodes()
    {
    // Static methods only
    }

  /**
   * Returns the {@link ObjectValue} represented by the given value
   */
  public static ObjectValue toObjectValue( Object value)
    {
    try
      {
      return toObjectValue( ObjectNode.class.cast( value));
      }
    catch( ClassCastException e)
      {
      throw new IllegalArgumentException( String.format( "value=%s is not an ObjectNode", value), e);
      }
    }

  /**
   * Returns the {@link ObjectValue} represented by the node
   */
  public static ObjectValue toObjectValue( ObjectNode node)
    {
    try
      {
      return ObjectValue.class.cast( toDataValue( node));
      }
    catch( ClassCastException e)
      {
      throw new IllegalArgumentException( String.format( "node=%s does not represent an ObjectValue", node), e);
      }
    }

  /**
   * Returns the {@link ArrayValue} represented by the given value
   */
  public static ArrayValue<Object> toArrayValue( Object value)
    {
    try
      {
      return toArrayValue( ArrayNode.class.cast( value));
      }
    catch( ClassCastException e)
      {
      throw new IllegalArgumentException( String.format( "value=%s is not an ArrayNode", value), e);
      }
    }

  /**
   * Returns the {@link ArrayValue} represented by the node
   */
  @SuppressWarnings("unchecked")
  public static ArrayValue<Object> toArrayValue( ArrayNode node)
    {
    try
      {
      return ArrayValue.class.cast( toDataValue( node));
      }
    catch( ClassCastException e)
      {
      throw new IllegalArgumentException( String.format( "node=%s does not represent an ArrayValue", node), e);
      }
    }

  /**
   * Returns the {@link DataValue} represented by the node
   */
  public static DataValue<?> toDataValue( JsonNode node)
    {
    DataValue<?> value;
    switch( node.getNodeType())
      {
      case ARRAY:
        {
        value = arrayOfAny( toStream( node.elements()).map( e -> toDataValue( e)).collect( toList()));
        break;
        }
      case BOOLEAN:
        {
        value = valueOf( node.asBoolean());
        break;
        }
      case NULL:
        {
        value = nullValue();
        break;
        }
      case NUMBER:
        {
        value =
          node.canConvertToInt()?
          valueOf( node.asInt()) :

          node.canConvertToLong()?
          valueOf( node.asLong()) :

          valueOf( node.decimalValue());
        break;
        }
      case OBJECT:
        {
        value =
          new ObjectValue(
            toStream( node.fieldNames())
            .collect( toMap( f -> f, f -> toDataValue( node.get(f)), (v1,v2) -> v1, LinkedHashMap::new)));
        break;
        }
      case STRING:
        {
        value = stringOf( node.asText());
        break;
        }
      default:
        {
        throw new IllegalArgumentException( String.format( "%s is not a supported JsonNode type", node.getNodeType()));
        }
      }
    
    return value;
    }
  }
