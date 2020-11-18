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
import org.cornutum.tcases.openapi.Characters;
import org.cornutum.tcases.openapi.resolver.DataValue.Type;

import java.util.AbstractMap.SimpleEntry;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import static java.util.Collections.emptyList;
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
      throw new IllegalStateException( String.format( "Can't get VarBinding for variable=%s", path), e);
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
      throw new IllegalStateException( String.format( "Can't get value set at path=%s", path), e);
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
      .orElseThrow( () -> new IllegalStateException( String.format( "Variable=%s is undefined", path)));
    }

  /**
   * Returns the value set at the given property path. Throws an exception if not such value set is defined.
   */
  public static Map<String,Object> expectPropertyValues( Map<String,Object> propertyValues, String path)
    {
    return
      Optional.ofNullable( getPropertyValues( propertyValues, path))
      .orElseThrow( () -> new IllegalStateException( String.format( "No value set defined at path=%s", path)));
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
   * If the given properties define one of multiple alternative values, returns the properties of
   * the specified alternative. Otherwise, returns <CODE>propertyValues</CODE>.
   */
  public static Map<String,Object> getAlternativePropertyValues( Map<String,Object> propertyValues)
    {
    return
      Optional.ofNullable( propertyValues)
      .flatMap( properties -> Optional.ofNullable( getPropertyValues( properties, "Alternative")))
      .map( alternatives -> getPropertyValues( alternatives, String.valueOf( getVarBinding( alternatives, "Used").getValue())))
      .orElse( propertyValues);
    }

  /**
   * Returns the accepted value types represented by the given properties.
   */
  public static Type[] getValueTypes( Map<String,Object> propertyValues)
    {
    Optional<VarBinding> typeDef =
      Optional.ofNullable( propertyValues)
      .flatMap( pv -> Optional.ofNullable( getVarBinding( pv, "Type")))
      .filter( td -> !td.isValueNA());
    
    Optional<String> typeValue =
      typeDef
      .flatMap( td -> Optional.ofNullable( td.getValue()).map( String::valueOf));

    Optional<String> excludedTypeValue =
      Optional.ofNullable( propertyValues)
      .flatMap( pv -> Optional.ofNullable( getVarBinding( pv, "Defined")))
      .map( defined -> defined.getAnnotation( "excludedType"));      

    Type[] types;
    if( propertyValues == null)
      {
      types = Type.any();
      }
    else if( !typeDef.isPresent())
      {
      types =
        excludedTypeValue.isPresent()
        ? getValueTypes( excludedTypeValue)
        : null;
      }
    else 
      {
      types = getValueTypes( typeValue);
      }

    return types;
    }

  /**
   * Returns the accepted value types represented by the given string.
   */
  private static Type[] getValueTypes( Optional<String> typeValue)
    {
    Type[] types;
    if( !typeValue.isPresent())
      {
      types = Type.only( Type.NULL);
      }
    else
      {
      Matcher matcher = valueTypePattern_.matcher( typeValue.get());
      if( !matcher.matches())
        {
        throw new IllegalStateException( String.format( "Unknown value type=%s", typeValue.get()));
        }

      Type[] baseTypes;
      try
        {
        baseTypes =
          Arrays.stream( matcher.group(2).toUpperCase().split( ","))
          .map( Type::valueOf)
          .toArray( Type[]::new);
        }
      catch( Exception e)
        {
        throw new IllegalStateException( "Unknown value type", e);
        }

      types =
        matcher.group(1) == null
        ? baseTypes
        : Type.not( baseTypes);
      }

    return types;
    }

  /**
   * Returns the value domain specified by the given properties.
   */
  public static ValueDomain<?> toValueDomain( Map<String,Object> propertyValues, Characters chars)
    {
    Map<String,Object> valueProperties =  getAlternativePropertyValues( propertyValues);
    
    Type[] types = getValueTypes( valueProperties);

    return
      types == null?
      null :
      
      types.length > 1?
      new MultiTypeDomain( chars, types) :

      types[0] == Type.ARRAY?
      toArrayDomain( valueProperties, chars) :

      types[0] == Type.BOOLEAN?
      toBooleanDomain( valueProperties) :

      types[0] == Type.INTEGER?
      toNumberDomain( Type.INTEGER, valueProperties) :

      types[0] == Type.NULL?
      new NullDomain() :

      types[0] == Type.NUMBER?
      toNumberDomain( Type.NUMBER, valueProperties) :

      types[0] == Type.OBJECT?
      toObjectDomain( valueProperties, chars) :

      types[0] == Type.STRING?
      toStringDomain( valueProperties, chars) :

      null;
    }

  /**
   * Returns the value domain for alternate array items specified by the given properties.
   */
  public static ValueDomain<?> toItemsDomain( Map<String,Object> propertyValues, Characters chars)
    {
    Map<String,Object> valueProperties =  getAlternativePropertyValues( propertyValues);

    Type[] types = 
      valueProperties == null?
      Type.any() :
      
      Optional.ofNullable( expectVarBinding( valueProperties, "Type").getAnnotation( "itemType"))
      .map( type -> getValueTypes( Optional.of( type)))
      .orElse( null);

    return
      types == null?
      null :
      
      types.length > 1?
      new MultiTypeDomain( chars, types) :

      types[0] == Type.ARRAY?
      toArrayItemsDomain( valueProperties, chars) :

      types[0] == Type.BOOLEAN?
      toBooleanItemsDomain( valueProperties) :

      types[0] == Type.INTEGER?
      toNumberItemsDomain( Type.INTEGER, valueProperties) :

      types[0] == Type.NUMBER?
      toNumberItemsDomain( Type.NUMBER, valueProperties) :

      types[0] == Type.OBJECT?
      new ObjectDomain( chars) :

      types[0] == Type.STRING?
      toStringItemsDomain( valueProperties, chars) :

      null;
    }

  /**
   * Returns the array domain specified by the given properties.
   */
  public static ValueDomain<?> toArrayDomain( Map<String,Object> propertyValues, Characters chars)
    {
    ValueDomain<?> domain;

    VarBinding value = getIfVarBinding( propertyValues, "Value");
    if( value != null)
      {
      domain = new ArrayConstant( JsonNodes.toArrayValue( value.getValue()));
      }
    else
      {
      ArrayDomain<?> arrayDomain;

      Map<String,Object> items = getPropertyValues( propertyValues, "Items");
      if( items == null)
        {
        arrayDomain = new ArrayDomain<Object>();
        }
      else
        {
        Map<String,Object> itemValueProperties = expectPropertyValues( items, "Contains");
        ValueDomain<?> otherItemValues = toItemsDomain( itemValueProperties, chars);
        ValueDomain<?> itemValues = toValueDomain( itemValueProperties, chars);
        ValueDomain<?> itemDomain = itemValues == null? otherItemValues : itemValues;
        arrayDomain = itemDomain.arrayOf();
        arrayDomain.setOtherItemValues( otherItemValues);

        VarBinding size = expectVarBinding( items, "Size");
        arrayDomain.setItemCount( Range.of( size));

        arrayDomain.setItemsUnique(
          Optional.ofNullable( getVarBinding( items, "Unique"))
          .filter( u -> !u.isValueNA())
          .map( u -> "Yes".equals( u.getValue()))
          .orElse( size.getAnnotation( "itemsUnique") != null));
        }

      domain = arrayDomain;
      }
    
    return domain;
    }

  /**
   * Returns the boolean domain specified by the given properties.
   */
  public static ValueDomain<?> toBooleanDomain( Map<String,Object> propertyValues)
    {
    return
      new BooleanConstant(
        Optional.ofNullable( getVarBinding( propertyValues, "Value"))
        .map( b -> (Boolean) b.getValue())
        .orElse( true));
    }

  /**
   * Returns the string domain specified by the given properties.
   */
  public static ValueDomain<?> toStringDomain( Map<String,Object> propertyValues, Characters chars)
    {
    String format;
    Set<String> excluded;

    Map<String,Object> stringProperties = getIfPropertyValues( propertyValues, "Value");

    VarBinding value =
      stringProperties == null
      ? Optional.ofNullable( getVarBinding( propertyValues, "Value")).orElse( null)
      : Optional.ofNullable( getVarBinding( stringProperties, "Is")).filter( v -> !v.isValueNA()).orElse( null);

    VarBinding length =
      Optional.ofNullable( stringProperties)
      .flatMap( properties -> Optional.of( expectVarBinding( properties, "Length")))
      .filter( binding -> !binding.isValueNA())
      .orElse( null);
    
    if( value != null)
      {
      format = value.getAnnotation( "format");
      excluded = Optional.ofNullable( value.getAnnotationList( "excluded")).map( e -> e.stream().collect( toSet())).orElse( emptySet());
      }
    else if( length != null)
      {
      format = length.getAnnotation( "format");
      excluded = emptySet();
      }
    else
      {
      format = null;
      excluded = emptySet();
      }

    ValueDomain<?> domain;
    if( value != null && excluded.isEmpty())
      {
      domain =
        "date".equals( format)?
        new DateConstant( String.valueOf( value.getValue())) :

        "date-time".equals( format)?
        new DateTimeConstant( String.valueOf( value.getValue())) :

        "uuid".equals( format)?
        new UuidConstant( String.valueOf( value.getValue())) :

        "binary".equals( format)?
        new BinaryConstant( (byte[]) value.getValue()) :

        "email".equals( format)?
        new EmailConstant( String.valueOf( value.getValue()), chars) :

        new StringConstant( String.valueOf( value.getValue()), format, chars);
      }
    else
      {
      SequenceDomain<?> baseDomain =
        "binary".equals( format)?
        new BinaryDomain() :

        "byte".equals( format)?
        new Base64Domain() :

        "date".equals( format)?
        withPatterns( stringProperties, new DateDomain()) :

        "date-time".equals( format)?
        withPatterns( stringProperties, new DateTimeDomain()) :

        "uuid".equals( format)?
        withPatterns( stringProperties, new UuidDomain()) :

        "email".equals( format)?
        withPatterns( stringProperties, new EmailDomain( chars)) :

        withPatterns( stringProperties, new AsciiStringDomain( chars));

      Optional.ofNullable( length)
        .map( Range::of)
        .ifPresent( range -> baseDomain.setLengthRange( range));

      baseDomain.setExcludedStrings( excluded);
      domain = baseDomain;
      }
    
    return domain;
    }

  /**
   * Returns the string domain with the specified pattern properties.
   */
  private static AbstractStringDomain withPatterns( Map<String,Object> stringProperties, AbstractStringDomain stringDomain)
    {
    List<VarBinding> matchingPatterns =
      Optional.ofNullable( stringProperties)
      .map(
        values ->
        // Single pattern match defined?
        Optional.ofNullable( getIfVarBinding( values, "Matches-Pattern"))
        .map( matchesPattern -> {
          // Yes, return binding as singleton list
          List<VarBinding> matchesPatterns =
            matchesPattern.isValueNA()
            ? emptyList()
            : Arrays.asList( matchesPattern);
          return matchesPatterns;
          })

        // Multiple pattern matches defined?
        .orElseGet(
          () ->
          Optional.ofNullable( getIfPropertyValues( values, "Matches-Patterns"))

          // Yes, return all bindings
          .map( matchesPatterns -> getVarBindings( matchesPatterns).filter( matchesPattern -> !matchesPattern.isValueNA()).collect( toList()))

          // No, no pattern matches defined
          .orElse( emptyList())))
      .orElse( emptyList());

    stringDomain.setMatching(
      matchingPatterns.stream()
      .filter( binding -> "Yes".equals( binding.getValue()))
      .map( binding -> binding.getAnnotation( "pattern"))
      .collect( toList()));

    stringDomain.setNotMatching(
      matchingPatterns.stream()
      .filter( binding -> "No".equals( binding.getValue()))
      .map( binding -> binding.getAnnotation( "pattern"))
      .collect( toList()));

    return stringDomain;
    }

  /**
   * Returns the object domain specified by the given properties.
   */
  public static ValueDomain<?> toObjectDomain( Map<String,Object> propertyValues, Characters chars)
    {
    ValueDomain<?> domain;

    VarBinding value = getIfVarBinding( propertyValues, "Value");
    if( value != null)
      {
      domain = new ObjectConstant( JsonNodes.toObjectValue( value.getValue()));
      }
    else
      {
      ObjectDomain objDomain = new ObjectDomain( chars);
      Map<String,Object> valueProperties = getPropertyValues( propertyValues, "Value");
      if( valueProperties != null)
        {
        Map<String,Object> properties = expectPropertyValues( valueProperties, "Properties");

        objDomain.setPropertyDomains(
          properties.keySet().stream()
          .filter( p -> !p.equals( "Additional"))
          .map( p -> new SimpleEntry<String,ValueDomain<?>>( p, toPropertyDomain( expectPropertyValues( properties, p), chars)))
          .filter( e -> e.getValue() != null)
          .collect( toMap( SimpleEntry::getKey, SimpleEntry::getValue)));

        Map<String,Object> additionalProperties = getIfPropertyValues( properties, "Additional");
        ValueDomain<?> additionalPropertyValues =
          additionalProperties != null?
          toPropertyDomain( additionalProperties, chars) :

          "Yes".equals( expectVarBinding( properties, "Additional").getValue())?
          new MultiTypeDomain( chars, Type.any()) :

          null;

        if( additionalPropertyValues != null)
          {
          objDomain.setAdditionalPropertyValues( additionalPropertyValues);

          IntegerDomain expectedPropertyCount =
            new IntegerDomain(
              Optional.ofNullable( getVarBinding( valueProperties, "Property-Count"))
              .map( Range::of)
              .orElse( Range.of( ">=", "0")));

          int definedPropertyCount = objDomain.getPropertyDomains().size();
          int expectedPropertyCountMin = expectedPropertyCount.getMin();
          int additionalPropertyCountMin = Math.max( 1, expectedPropertyCountMin - definedPropertyCount);

          int expectedPropertyCountMax = expectedPropertyCount.getMax();
          int additionalPropertyCountMax =
            expectedPropertyCountMax == expectedPropertyCount.getMaxRange()
            ? additionalPropertyCountMin + 2
            : expectedPropertyCountMax - definedPropertyCount;

          objDomain.setAdditionalPropertyCount( new IntegerDomain( additionalPropertyCountMin, additionalPropertyCountMax));
          }
        }
    
      domain = objDomain;
      }

    return domain;
    }

  /**
   * Returns the object property domain specified by the given properties.
   */
  private static ValueDomain<?> toPropertyDomain( Map<String,Object> propertyValues, Characters chars)
    {
    return
      "Yes".equals( expectVarBinding( propertyValues, "Defined").getValue())
      ? toValueDomain( propertyValues, chars)
      : null;
    }

  /**
   * Returns the number domain specified by the given properties.
   */
  public static ValueDomain<?> toNumberDomain( Type type, Map<String,Object> propertyValues)
    {
    String format;
    Range range;
    boolean constant;

    Map<String,Object> valueProperties = getPropertyValues( propertyValues, "Value");
    if( valueProperties == null)
      {
      format = null;
      range = null;
      constant = false;
      }
    else
      {
      VarBinding is = expectVarBinding( valueProperties, "Is");
      format = is.getAnnotation( "format");
      range = Range.of( is);
      constant = range.isConstant();
      }

    ValueDomain<?> valueDomain;
    if( constant)
      {
      valueDomain = 
        type == Type.NUMBER?
        new DecimalConstant( new BigDecimal( range.getMax()), format) :

        "int64".equals( format)?
        new LongConstant( Long.valueOf( range.getMax())) :

        new IntegerConstant( Integer.valueOf( range.getMax()));
      }
    else
      {
      NumberDomain<?> numberDomain =       
        type == Type.NUMBER?
        new DecimalDomain( range, format) :
        
        "int64".equals( format)?
        new LongDomain( range) :
        
        new IntegerDomain( range);

      Map<String,Object> multipleOfValues =
        Optional.ofNullable( valueProperties)
        .flatMap( value -> Optional.ofNullable( getPropertyValues( value, "Multiple-Of")))
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

  /**
   * Returns the array domain specified by the given properties.
   */
  public static ValueDomain<?> toArrayItemsDomain( Map<String,Object> propertyValues, Characters chars)
    {
    ArrayDomain<?> domain;

    Map<String,Object> items = getPropertyValues( propertyValues, "Items");
    if( items == null)
      {
      domain = new ArrayDomain<Object>();
      }
    else
      {
      Map<String,Object> itemValueProperties = expectPropertyValues( items, "Contains");
      ValueDomain<?> otherItemValues = toItemsDomain( itemValueProperties, chars);
      ValueDomain<?> itemValues = toValueDomain( itemValueProperties, chars);
      ValueDomain<?> itemDomain = itemValues == null? otherItemValues : itemValues;
      domain = itemDomain.arrayOf();

      VarBinding size = expectVarBinding( items, "Size");
      domain.setItemCount(
        Optional.ofNullable( size.getAnnotation( "itemMinItems")).map( Integer::valueOf).orElse( null),
        Optional.ofNullable( size.getAnnotation( "itemMaxItems")).map( Integer::valueOf).orElse( null));
      }

    return domain;
    }

  /**
   * Returns the boolean domain specified by the given properties.
   */
  public static ValueDomain<?> toBooleanItemsDomain( Map<String,Object> propertyValues)
    {
    return
      Optional.ofNullable( getVarBinding( propertyValues, "Value").getAnnotationList( "itemEnums"))
      .map( enums -> new BooleanEnum( enums))
      .orElse( null);
    }

  /**
   * Returns the string domain specified by the given properties.
   */
  public static ValueDomain<?> toStringItemsDomain( Map<String,Object> propertyValues, Characters chars)
    {
    ValueDomain<?> domain;

    VarBinding valueVar = getIfVarBinding( propertyValues, "Value");
    if( valueVar != null)
      {
      String format = valueVar.getAnnotation( "format");
      Iterable<String> enums = valueVar.getAnnotationList( "itemEnums");
      domain =
        "date".equals( format)?
        new DateEnum( enums) :

        "date-time".equals( format)?
        new DateTimeEnum( enums) :

        "uuid".equals( format)?
        new UuidEnum( enums) :

        "email".equals( format)?
        new EmailEnum( enums, chars) :

        new StringEnum( enums, format, chars);
      }
    else
      {
      VarBinding lengthVar = expectVarBinding( expectPropertyValues( propertyValues, "Value"), "Length");
      String format = lengthVar.getAnnotation( "format");
      List<String> patterns = lengthVar.getAnnotationList( "itemPatterns");
      List<String> notPatterns = lengthVar.getAnnotationList( "itemPatterns");
      
      SequenceDomain<?> baseDomain =
        "binary".equals( format)?
        new BinaryDomain() :

        "byte".equals( format)?
        new Base64Domain() :

        "date".equals( format)?
        withItemPatterns( patterns, notPatterns, new DateDomain()) :

        "date-time".equals( format)?
        withItemPatterns( patterns, notPatterns, new DateTimeDomain()) :

        "uuid".equals( format)?
        withItemPatterns( patterns, notPatterns, new UuidDomain()) :

        "email".equals( format)?
        withItemPatterns( patterns, notPatterns, new EmailDomain( chars)) :

        withItemPatterns( patterns, notPatterns, new AsciiStringDomain( chars));

      Integer minLength = Optional.ofNullable( lengthVar.getAnnotation( "itemMinLength")).map( Integer::valueOf).orElse( null);
      Integer maxLength = Optional.ofNullable( lengthVar.getAnnotation( "itemMaxLength")).map( Integer::valueOf).orElse( null);
      baseDomain.setLengthRange( minLength, maxLength);

      Set<String> excluded = Optional.ofNullable( lengthVar.getAnnotationList( "itemNotEnums")).map( e -> e.stream().collect( toSet())).orElse( emptySet());;
      baseDomain.setExcludedStrings( excluded);
      
      domain = baseDomain;
      }

    return domain;
    }

  /**
   * Returns the string domain with the specified pattern properties.
   */
  private static AbstractStringDomain withItemPatterns( List<String> patterns, List<String> notPatterns, AbstractStringDomain stringDomain)
    {
    if( patterns != null)
      {
      stringDomain.setMatching( patterns);
      }
    if( notPatterns != null)
      {
      stringDomain.setNotMatching( notPatterns);
      }
    
    return stringDomain;
    }

  /**
   * Returns the number domain specified by the given properties.
   */
  public static ValueDomain<?> toNumberItemsDomain( Type type, Map<String,Object> propertyValues)
    {
    Optional<VarBinding> is =
      Optional.ofNullable( getPropertyValues( propertyValues, "Value"))
      .map( valueProperties -> expectVarBinding( valueProperties, "Is"));

    String format = is.map( binding -> binding.getAnnotation( "format")).orElse( null);
    List<String> enums = is.map( binding -> binding.getAnnotationList( "itemEnums")).orElse( null);

    ValueDomain<?> valueDomain;
    if( enums != null)
      {
      valueDomain = 
        type == Type.NUMBER?
        new DecimalEnum( enums, format) :

        "int64".equals( format)?
        new LongEnum( enums) :

        new IntegerEnum( enums);
      }
    else
      {
      String min =
        is.flatMap( binding -> Optional.ofNullable( binding.getAnnotation( "itemMin")))
        .orElse( null);
      
      String max =
        is.flatMap( binding -> Optional.ofNullable( binding.getAnnotation( "itemMax")))
        .orElse( null);

      Set<String> excluded = 
        is.flatMap( binding -> Optional.ofNullable( binding.getAnnotationList( "itemNotEnums")))
        .map( List::stream)
        .orElse( Stream.empty())
        .collect( toSet());

      Range range = new Range( min, false, max, false, excluded);
      
      NumberDomain<?> numberDomain =       
        type == Type.NUMBER?
        new DecimalDomain( range, format) :
        
        "int64".equals( format)?
        new LongDomain( range) :
        
        new IntegerDomain( range);

      is.flatMap( binding -> Optional.ofNullable( binding.getAnnotation( "itemMultipleOf")))
        .ifPresent( multipleOf -> numberDomain.setMultipleOf( multipleOf));
      
      is.flatMap( binding -> Optional.ofNullable( binding.getAnnotationList( "itemNotMultipleOfs")))
        .ifPresent( notMultipleOfs -> numberDomain.setNotMultipleOfs( notMultipleOfs.stream().toArray( String[]::new)));

      valueDomain = numberDomain;
      }

    return valueDomain;
    }

  private final static Pattern valueTypePattern_ = Pattern.compile( "(Not )?(.*)");
  }
