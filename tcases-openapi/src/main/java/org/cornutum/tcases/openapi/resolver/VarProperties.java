//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.DefUtils;
import org.cornutum.tcases.VarBinding;
import org.cornutum.tcases.openapi.resolver.NumberDomain.Range;
import org.cornutum.tcases.openapi.resolver.ValueDomain.Type;

import java.util.AbstractMap.SimpleEntry;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.toSet;

import java.math.BigDecimal;

/**
 * Defines methods for extracting the properties of test case input variables.
 */
public final class VarProperties
  {
  /**
   * Creates a new VarProperties instance.
   */
  private VarProperties()
    {
    // Static methods only
    }

  /**
   * Transforms a set of variable bindings into a map of property values. 
   */
  public static Map<String,Object> getPropertyValues( List<VarBinding> bindings)
    {
    Map<String,Object> propertyValues = new LinkedHashMap<String,Object>();
    bindings.stream().forEach( binding -> putPropertyValue( propertyValues, getPathRest( getVarPath( binding)), binding));
    return propertyValues;
    }

  /**
   * Sets the property at the given path to the given variable binding.
   */
  @SuppressWarnings("unchecked")
  public static void putPropertyValue( Map<String,Object> propertyValues, List<String> path, VarBinding binding)
    {
    String pathFirst = getPathFirst( path);
    if( path.size() == 1)
      {
      propertyValues.put( pathFirst, binding);
      }
    else
      {
      Map<String,Object> restValues =
        Optional.ofNullable( (Map<String,Object>) propertyValues.get( pathFirst))
        .orElse( new LinkedHashMap<String,Object>());

      propertyValues.put( pathFirst, restValues);
      putPropertyValue( restValues, getPathRest( path), binding);
      }
    }

  /**
   * Returns the variable binding at the given property path.
   */
  public static VarBinding getVarBinding( Map<String,Object> propertyValues, String path)
    {
    try
      {
      return (VarBinding) getPropertyValue( propertyValues, path);
      }
    catch( Exception e)
      {
      throw new RequestCaseException( String.format( "Can't get VarBinding for variable=%s", path), e);
      }
    }

  /**
   * Returns a variable binding if it is referenced by the given property path. Otherwise returns null.
   */
  public static VarBinding getIfVarBinding( Map<String,Object> propertyValues, String path)
    {
    try
      {
      return getVarBinding( propertyValues, path);
      }
    catch( Exception e)
      {
      return null;
      }
    }

  /**
   * Returns the value set at the given property path.
   */
  @SuppressWarnings("unchecked")
  public static Map<String,Object> getPropertyValues( Map<String,Object> propertyValues, String path)
    {
    try
      {
      return (Map<String,Object>) getPropertyValue( propertyValues, path);
      }
    catch( Exception e)
      {
      throw new RequestCaseException( String.format( "Can't get value set at path=%", path), e);
      }
    }

  /**
   * Returns a value set if it is referenced by the given property path. Otherwise returns null.
   */
  public static Map<String,Object> getIfPropertyValues( Map<String,Object> propertyValues, String path)
    {
    try
      {
      return getPropertyValues( propertyValues, path);
      }
    catch( Exception e)
      {
      return null;
      }
    }

  /**
   * Returns the variable binding at the given property path. Throws an exception if not such binding is defined.
   */
  public static VarBinding expectVarBinding( Map<String,Object> propertyValues, String path)
    {
    return
      Optional.ofNullable( getVarBinding( propertyValues, path))
      .orElseThrow( () -> new RequestCaseException( String.format( "Variable=%s is undefined", path)));
    }

  /**
   * Returns the value set at the given property path. Throws an exception if not such value set is defined.
   */
  public static Map<String,Object> expectPropertyValues( Map<String,Object> propertyValues, String path)
    {
    return
      Optional.ofNullable( getPropertyValues( propertyValues, path))
      .orElseThrow( () -> new RequestCaseException( String.format( "No value set defined at path=%s", path)));
    }

  /**
   * Returns the value at the given property path.
   */
  public static Object getPropertyValue( Map<String,Object> propertyValues, String path)
    {
    return getPropertyValue( propertyValues, getValuePath( path));
    }

  /**
   * Returns the value at the given property path.
   */
  @SuppressWarnings("unchecked")
  public static Object getPropertyValue( Map<String,Object> propertyValues, List<String> valuePath)
    {
    int valuePathSize =
      valuePath == null
      ? 0
      : valuePath.size();
    
    Object valueFirst =
      valuePathSize > 0
      ? propertyValues.get( getPathFirst( valuePath))
      : null;

    return
      valuePathSize == 1?
      valueFirst :

      valueFirst != null?
      getPropertyValue( (Map<String,Object>) valueFirst, getPathRest( valuePath)) :

      null;
    }

  /**
   * Returns the variable bindings specified by the given property values.
   */
  @SuppressWarnings("unchecked")
  public static Stream<VarBinding> getVarBindings( Map<String,Object> propertyValues)
    {
    return
      propertyValues.values().stream()
      .flatMap(
        value ->
        value instanceof VarBinding
        ? Stream.of( (VarBinding) value)
        : getVarBindings( (Map<String,Object>) value));
    }

  /**
   * Returns the variable set path for the given variable binding.
   */
  public static List<String> getVarPath( VarBinding binding)
    {
    return getValuePath( binding.getVar());
    }

  /**
   * Returns the list of elements in given value path.
   */
  public static List<String> getValuePath( String path)
    {
    return
      path == null
      ? null
      : Arrays.stream( DefUtils.toPath( path)).collect( toList());
    }

  /**
   * Returns the first name in the given variable set path
   */
  public static String getPathFirst( List<String> path)
    {
    return path.get(0);
    }

  /**
   * Returns the subpath of the given variable set path.
   */
  public static List<String> getPathRest( List<String> path)
    {
    return path.subList( 1, path.size());
    }

  /**
   * Returns the accepted value types represented by the given properties.
   */
  public static Type[] getValueTypes( Map<String,Object> propertyValues)
    {
    Type[] types;
    if( propertyValues == null)
      {
      types = Type.any();
      }
    else
      {
      String typeValue = Objects.toString( expectVarBinding( propertyValues, "Type").getValue(), null);
    
      if( typeValue == null)
        {
        types = Type.only( Type.NULL);
        }
      else
        {
        Matcher matcher = valueTypePattern_.matcher( typeValue);
        if( !matcher.matches())
          {
          throw new RequestCaseException( String.format( "Unknown value type=%", typeValue));
          }

        Type baseType;
        try
          {
          baseType = Type.valueOf( matcher.group(2).toUpperCase());
          }
        catch( Exception e)
          {
          throw new RequestCaseException( "Unknown value type", e);
          }

        types =
          matcher.group(1) == null
          ? Type.only( baseType)
          : Type.not( baseType);
        }
      }

    return types;
    }

  /**
   * Returns the value domain specified by the given properties.
   */
  public static ValueDomain<?> toValueDomain( Map<String,Object> propertyValues)
    {
    Type[] types = getValueTypes( propertyValues);

    return
      types.length > 1?
      new MultiTypeDomain( types) :

      types[0] == Type.ARRAY?
      toArrayDomain( propertyValues) :

      types[0] == Type.BOOLEAN?
      new BooleanConstant( (Boolean) expectVarBinding( propertyValues, "Value").getValue()) :

      types[0] == Type.INTEGER?
      toNumberDomain( Type.INTEGER, propertyValues) :

      types[0] == Type.NULL?
      new NullDomain() :

      types[0] == Type.NUMBER?
      toNumberDomain( Type.NUMBER, propertyValues) :

      types[0] == Type.OBJECT?
      toObjectDomain( propertyValues) :

      types[0] == Type.STRING?
      toStringDomain( propertyValues) :

      null;
    }

  /**
   * Returns the array domain specified by the given properties.
   */
  public static ArrayDomain<?> toArrayDomain( Map<String,Object> propertyValues)
    {
    return new ArrayDomain<Object>( propertyValues);
    }

  /**
   * Returns the string domain specified by the given properties.
   */
  public static ValueDomain<?> toStringDomain( Map<String,Object> propertyValues)
    {
    String format;
    Range lengthRange;
    Set<String> excluded;

    VarBinding valueBinding = getIfVarBinding( propertyValues, "Value");
    if( valueBinding != null)
      {
      format = valueBinding.getAnnotation( "format");
      lengthRange = null;
      excluded = Optional.ofNullable( valueBinding.getAnnotationList( "excluded")).map( e -> e.stream().collect( toSet())).orElse( emptySet());
      }
    else
      {
      Map<String,Object> stringProperties = expectPropertyValues( propertyValues, "Value");
      VarBinding length = expectVarBinding( stringProperties, "Length");
      format = length.getAnnotation( "format");
      lengthRange = Range.of( length);
      excluded = Optional.ofNullable( length.getAnnotationList( "excluded")).map( e -> e.stream().collect( toSet())).orElse( emptySet());
      }

    ValueDomain<?> domain;
    if( valueBinding != null && excluded.isEmpty())
      {
      domain =
        "date".equals( format)?
        new DateConstant( String.valueOf( valueBinding.getValue())) :

        "date-time".equals( format)?
        new DateTimeConstant( String.valueOf( valueBinding.getValue())) :

        "binary".equals( format)?
        new BinaryConstant( (byte[]) valueBinding.getValue()) :

        new StringConstant( String.valueOf( valueBinding.getValue()));
      }
    else
      {
      SequenceDomain<?> baseDomain =
        "date".equals( format)?
        new DateDomain() :

        "date-time".equals( format)?
        new DateTimeDomain() :

        "binary".equals( format)?
        new BinaryDomain() :

        "byte".equals( format)?
        new Base64Domain() :

        new AsciiStringDomain();

      if( !("date".equals( format) || "date-time".equals( format)))
        {
        baseDomain.setLengthRange( lengthRange);
        }
      baseDomain.setExcludedStrings( excluded);
      domain = baseDomain;
      }
    
    return domain;
    }

  /**
   * Returns the object domain specified by the given properties.
   */
  public static ValueDomain<?> toObjectDomain( Map<String,Object> propertyValues)
    {
    ObjectDomain domain = new ObjectDomain();

    Map<String,Object> value = expectPropertyValues( propertyValues, "Value");
    Map<String,Object> properties = expectPropertyValues( value, "Properties");

    domain.setPropertyDomains(
      properties.keySet().stream()
      .filter( p -> !p.equals( "Additional"))
      .map( p -> new SimpleEntry<String,ValueDomain<?>>( p, toPropertyDomain( expectPropertyValues( properties, p))))
      .filter( e -> e.getValue() != null)
      .collect( toMap( SimpleEntry::getKey, SimpleEntry::getValue)));

    Map<String,Object> additionalProperties = getIfPropertyValues( properties, "Additional");
    ValueDomain<?> additionalPropertyValues =
      additionalProperties != null?
      toPropertyDomain( additionalProperties) :

      "Yes".equals( expectVarBinding( properties, "Additional").getValue())?
      new MultiTypeDomain( Type.any()) :

      null;

    if( additionalPropertyValues != null)
      {
      domain.setAdditionalPropertyValues( additionalPropertyValues);

      IntegerDomain expectedPropertyCount =
        new IntegerDomain(
          Optional.ofNullable( getVarBinding( value, "Property-Count"))
          .map( Range::of)
          .orElse( Range.of( ">=", "0")));

      int definedPropertyCount = domain.getPropertyDomains().size();
      int expectedPropertyCountMin = expectedPropertyCount.getMin();
      int additionalPropertyCountMin = Math.max( 1, expectedPropertyCountMin - definedPropertyCount);

      int expectedPropertyCountMax = expectedPropertyCount.getMax();
      int additionalPropertyCountMax =
        expectedPropertyCountMax == expectedPropertyCount.getMaxRange()
        ? additionalPropertyCountMin + 2
        : expectedPropertyCountMax - definedPropertyCount;

      domain.setAdditionalPropertyCount( new IntegerDomain( additionalPropertyCountMin, additionalPropertyCountMax));
      }
    
    return domain;
    }

  /**
   * Returns the object property domain specified by the given properties.
   */
  private static ValueDomain<?> toPropertyDomain( Map<String,Object> propertyValues)
    {
    return
      "Yes".equals( expectVarBinding( propertyValues, "Defined").getValue())
      ? toValueDomain( propertyValues)
      : null;
    }

  /**
   * Returns the number domain specified by the given properties.
   */
  public static ValueDomain<?> toNumberDomain( Type type, Map<String,Object> propertyValues)
    {
    Map<String,Object> valueProperties = expectPropertyValues( propertyValues, "Value");
    VarBinding is = expectVarBinding( valueProperties, "Is");
    String format = is.getAnnotation( "format");
    Range range = Range.of( is);

    ValueDomain<?> valueDomain;
    if( range.isConstant())
      {
      valueDomain = 
        type == Type.INTEGER?
        ("int64".equals( format)?
         new LongConstant( Long.valueOf( range.getMax())) :
         new IntegerConstant( Integer.valueOf( range.getMax()))) :

         new DecimalConstant( new BigDecimal( range.getMax()));
      }
    else
      {
      NumberDomain<?> numberDomain =       
        type == Type.NUMBER?
        new DecimalDomain() :
        
        "int64".equals( format)?
        new LongDomain() :
        
        new IntegerDomain();

      numberDomain.setRange( range);
    
      Map<String,Object> multipleOfValues =
        Optional.ofNullable( getPropertyValues( propertyValues, "Multiple-Of"))
        .orElse( emptyMap());

      multipleOfValues.keySet().stream()
        .map( m -> expectVarBinding( multipleOfValues, m))
        .filter( binding -> "Yes".equals( binding.getValue()))
        .map( binding -> binding.getAnnotation( "multipleOf"))
        .findAny().ifPresent( m -> numberDomain.setMultipleOf( m));

      numberDomain.setNotMultipleOfs(
        multipleOfValues.keySet().stream()
        .map( m -> expectVarBinding( multipleOfValues, m))
        .filter( binding -> "No".equals( binding.getValue()))
        .map( binding -> binding.getAnnotation( "multipleOf"))
        .toArray( String[]::new));

      valueDomain = numberDomain;
      }

    return valueDomain;
    }

  private final static Pattern valueTypePattern_ = Pattern.compile( "(Not )?(.*)");
  }
