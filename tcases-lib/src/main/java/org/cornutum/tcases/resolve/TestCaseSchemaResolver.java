//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.*;
import static org.cornutum.tcases.VarValueDef.Type.FAILURE;
import static org.cornutum.tcases.resolve.DataValue.Type.*;
import static org.cornutum.tcases.resolve.DataValues.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.cornutum.regexpgen.RegExpGen;
import org.cornutum.regexpgen.js.Provider;
import static org.cornutum.regexpgen.Bounds.bounded;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Optional;
import java.util.stream.Stream;
import static java.math.RoundingMode.*;
import static java.util.stream.Collectors.toList;

/**
 * Uses the {@link Schema} definitions in {@link ITestCaseDef test case definitions} to create new {@link TestCase} instances.
 */
public class TestCaseSchemaResolver extends TestCaseResolver
  {
  /**
   * Creates a new TestCaseSchemaResolver instance.
   */
  public TestCaseSchemaResolver()
    {
    super();
    }
  
  /**
   * Creates a new TestCaseSchemaResolver instance.
   */
  public TestCaseSchemaResolver( ResolverContext context)
    {
    super( context);
    }

  /**
   * Resolves an input value definition.
   */
  @Override
  protected VarValueDef resolveValueDef( VarValueDef valueDef)
    {
    return valueDef;
    }
  
  /**
   * Prepare for resolution of input value definitions
   */
  @Override
  protected FunctionInputDef prepareValueDefs( FunctionInputDef inputDef)
    {
    FunctionInputDef prepared = FunctionInputDefBuilder.with( inputDef).build();
    
    getContext().doFor(
      prepared.getName(),
      () ->
      toStream( new VarDefIterator( prepared))
      .map( this::addValueDefs)
      .forEach( varDef -> prepareValueDefs( varDef)));

    return prepared;
    }
  
  /**
   * Prepare for resolution of input value definitions
   */
  private void prepareValueDefs( VarDef varDef)
    {
    getContext().doFor(
      varDef.getPathName(),
      () -> 
      toStream( varDef.getValues())
      .forEach( valueDef -> prepareValueDef( valueDef, Schema.merge( varDef.getSchema(), valueDef.getSchema()))));
    }
  
  /**
   * Prepare for resolution of the give value definition.
   */
  private void prepareValueDef( VarValueDef valueDef, Schema schema)
    {
    valueDef.setDomain(
      getContext().resultFor(
        String.valueOf( valueDef.getName()),
        () -> toValueDomain( normalize( schema))));
    }

  /**
   * If necessary, update the given variable with value definitions generated from the variable schema.
   */
  private VarDef addValueDefs( VarDef varDef)
    {
    if( !varDef.getValues().hasNext())
      {
      getContext().doFor(
        varDef.getPathName(),
        () ->
        valuesForSchema( varDef.getSchema())
        .forEach( value -> varDef.addValue( value)));
      }

    return varDef;
    }

  /**
   * Returns value definitions derived from the given {@link Schema}.
   */
  protected Stream<VarValueDef> valuesForSchema( Schema schema)
    {
    normalize( schema);
    
    return
      schema.getConstant() != null
      ? Stream.of( new VarValueDef( valueObject( schema.getConstant())))
      : valuesForSchema( schema.getType(), schema);
    }

  /**
   * Returns value object represent by the given {@link DataValue}.
   */
  private Object valueObject( DataValue<?> dataValue)
    {
    return
      dataValue.getType() == ARRAY
      ? ((ArrayValue<?>) dataValue).getValue().stream().map( this::valueObject).collect( toList())
      : dataValue.getValue();
    }

  /**
   * Returns value definitions derived from the given {@link Schema}.
   */
  private Stream<VarValueDef> valuesForSchema( DataValue.Type type, Schema schema)
    {
    Stream.Builder<VarValueDef> values = Stream.builder();
    switch( type)
      {
      case ARRAY:
        {
        addArrayValues( values, schema);
        break;
        }
      case BOOLEAN:
        {
        addBooleanValues( values, schema);
        break;
        }
      case INTEGER:
      case NUMBER:
        {
        addNumberValues( values, schema);
        break;
        }
      case STRING:
        {
        addStringValues( values, schema);
        break;
        }
      default:
        {
        break;
        }
      }
    
    return values.build();
    }

  /**
   * Adds value definitions derived from an array {@link Schema} to the given stream.
   */
  private void addArrayValues( Stream.Builder<VarValueDef> values, Schema schema)
    {
    Integer minItems = schema.getMinItems();
    int effMinItems = Optional.ofNullable( minItems).orElse( 0);
    if( minItems == null)
      {
      values.add(
        VarValueDefBuilder.with( "empty")
        .schema(
          SchemaBuilder.type( "array")
          .format( schema.getFormat())
          .maxItems(0)
          .build())
        .build());
      }
    else
      {
      values.add(
        VarValueDefBuilder.with( "minimumSize")
        .schema( SchemaBuilder.with( schema).maxItems( minItems).build())
        .build());

      int belowMin = minItems - 1;
      if( belowMin >= 0)
        {
        values.add(
          VarValueDefBuilder.with( "tooSmall")
          .type( FAILURE)
          .schema( SchemaBuilder.with( schema).minItems( 0).maxItems( belowMin).build())
          .build());
        }
      }

    Integer maxItems = schema.getMaxItems();
    if( maxItems == null)
      {
      values.add(
        VarValueDefBuilder.with( "anySize")
        .schema( SchemaBuilder.with( schema).minItems( effMinItems + 1).build())
        .build());
      }
    else 
      {
      if( maxItems != effMinItems)
        {
        values.add(
          VarValueDefBuilder.with( "maximumSize")
          .schema( SchemaBuilder.with( schema).minItems( maxItems).maxItems( maxItems).build())
          .build());
        }

      int aboveMax = maxItems + 1;
      values.add(
        VarValueDefBuilder.with( "tooLarge")
        .type( FAILURE)
        .schema( SchemaBuilder.with( schema).minItems( aboveMax).maxItems( null).build())
        .build());
      }

    if( Optional.ofNullable( schema.getUniqueItems()).orElse( false)
        && Optional.ofNullable( schema.getMaxItems()).map( max -> max > 1).orElse( true))
      {
      int minUnique = Math.max( effMinItems, 2);
      
      values.add(
        VarValueDefBuilder.with( "notUnique")
        .type( FAILURE)
        .schema(
          SchemaBuilder.with( schema)
          .uniqueItems( false)
          .minItems( minUnique)
          .build())
        .build());
      }
    }

  /**
   * Adds value definitions derived from a boolean {@link Schema} to the given stream.
   */
  private void addBooleanValues( Stream.Builder<VarValueDef> values, Schema schema)
    {
    values.add( new VarValueDef( Boolean.TRUE));
    values.add( new VarValueDef( Boolean.FALSE));
    }

  /**
   * Adds value definitions derived from a numeric {@link Schema} to the given stream.
   */
  private void addNumberValues( Stream.Builder<VarValueDef> values, Schema schema)
    {
    BigDecimal minimum = schema.getMinimum();
    BigDecimal maximum = schema.getMaximum();
    if( minimum == null && maximum == null)
      {
      values.add(
        VarValueDefBuilder.with( "negative")
        .schema( SchemaBuilder.with( schema).exclusiveMaximum( BigDecimal.ZERO).build())
        .build());
      values.add(
        VarValueDefBuilder.with( "zero")
        .schema( SchemaBuilder.type( "number").constant( BigDecimal.ZERO).build())
        .build());
      values.add(
        VarValueDefBuilder.with( "positive")
        .schema( SchemaBuilder.with( schema).exclusiveMinimum( BigDecimal.ZERO).build())
        .build());
      }
    else
      {
      if( minimum != null)
        {
        values.add(
          VarValueDefBuilder.with( "minimum")
          .schema( SchemaBuilder.with( schema).maximum( minimum).build())
          .build());

        values.add(
          VarValueDefBuilder.with( "belowMinimum")
          .type( FAILURE)
          .schema(
            SchemaBuilder.with( schema)
            .exclusiveMaximum( minimum)
            .minimum( bigDecimalNull())
            .maximum( bigDecimalNull())
            .build())
          .build());
        }
      else
        {
        values.add(
          VarValueDefBuilder.with( "belowMaximum")
          .schema(
            SchemaBuilder.with( schema)
            .exclusiveMaximum( maximum)
            .maximum( bigDecimalNull())
            .build())
          .build());
        }

      if( maximum != null)
        {
        if( !(minimum != null && minimum.compareTo( maximum) == 0))
          {
          values.add(
            VarValueDefBuilder.with( "maximum")
            .schema( SchemaBuilder.with( schema).minimum( maximum).build())
            .build());
          }

        values.add(
          VarValueDefBuilder.with( "aboveMaximum")
          .type( FAILURE)
          .schema(
            SchemaBuilder.with( schema)
            .exclusiveMinimum( maximum)
            .minimum( bigDecimalNull())
            .maximum( bigDecimalNull())
            .build())
          .build());
        }
      else
        {
        values.add(
          VarValueDefBuilder.with( "aboveMinimum")
          .schema(
            SchemaBuilder.with( schema)
            .exclusiveMinimum( minimum)
            .minimum( bigDecimalNull())
            .build())
          .build());
        }
      }

    Optional.ofNullable( schema.getMultipleOf())
      .flatMap( multipleOf -> {
        BigDecimal unit = unitOf( schema);
        BigDecimal factor = multipleOf.compareTo( unit) == 0? multipleOf.divide( new BigDecimal( 2)) : unit;

        boolean multiple;
        BigDecimal notMultiple;
        if( maximum != null)
          {
          for(
            multiple = true,
              notMultiple = maximum.subtract( factor);
            
            (minimum == null || notMultiple.compareTo( minimum) > 0)
              && (multiple = isMultipleOf( notMultiple, multipleOf));
            
            notMultiple = notMultiple.subtract( factor));
          }
        else
          {
          BigDecimal floor = Optional.ofNullable( minimum).orElse( BigDecimal.ZERO);
          for(
            multiple = true,
              notMultiple = floor.add( factor);
            
            (maximum == null || notMultiple.compareTo( maximum) < 0)
              && (multiple = isMultipleOf( notMultiple, multipleOf));
            
            notMultiple = notMultiple.add( factor));
          }

        return
          !multiple
          ? Optional.of( notMultiple)
          : Optional.empty();
        })
      .ifPresent( notMultiple -> {
        values.add(
          VarValueDefBuilder.with( "notMultiple")
          .type( FAILURE)
          .schema( SchemaBuilder.type( "number").constant( notMultiple).build())
          .build());
        });

    }

  /**
   * Adds value definitions derived from a string {@link Schema} to the given stream.
   */
  private void addStringValues( Stream.Builder<VarValueDef> values, Schema schema)
    {
    Integer[] lengthRequired = lengthRequired( schema); 
    Integer minRequired = lengthRequired[0];
    Integer maxRequired = lengthRequired[1];

    Integer minLength = schema.getMinLength();
    int effMinLength = Optional.ofNullable( minLength).orElse( 0);
    if( minLength == null)
      {
      values.add(
        VarValueDefBuilder.with( "empty")
        .schema(
          SchemaBuilder.type( "string")
          .format( schema.getFormat())
          .maxLength(0)
          .build())
        .build());
      }
    else
      {
      values.add(
        VarValueDefBuilder.with( "minimumLength")
        .schema( SchemaBuilder.with( schema).maxLength( minLength).build())
        .build());

      int belowMin = minLength - 1;
      if( belowMin >= 0 && Optional.ofNullable( minRequired).map( reqMin -> belowMin >= reqMin).orElse( true))
        {
        values.add(
          VarValueDefBuilder.with( "tooShort")
          .type( FAILURE)
          .schema( SchemaBuilder.with( schema).minLength( null).maxLength( belowMin).build())
          .build());
        }
      }

    Integer maxLength = schema.getMaxLength();
    if( maxLength == null)
      {
      values.add(
        VarValueDefBuilder.with( "anyLength")
        .schema( SchemaBuilder.with( schema).minLength( effMinLength + 1).build())
        .build());
      }
    else 
      {
      if( maxLength != effMinLength)
        {
        values.add(
          VarValueDefBuilder.with( "maximumLength")
          .schema( SchemaBuilder.with( schema).minLength( maxLength).maxLength( maxLength).build())
          .build());
        }

      int aboveMax = maxLength + 1;
      if( Optional.ofNullable( maxRequired).map( reqMax -> aboveMax <= reqMax).orElse( true))
        {
        values.add(
          VarValueDefBuilder.with( "tooLong")
          .type( FAILURE)
          .schema( SchemaBuilder.with( schema).minLength( aboveMax).maxLength( null).build())
          .build());
        }
      }

    Optional.ofNullable( schema.getFormat())
      .filter( format -> isPatternedFormat( format))
      .map( format -> format.equals( "email")? "date-time" : "email")
      .ifPresent( other -> {
        values.add(
          VarValueDefBuilder.with( "wrongFormat")
          .type( FAILURE)
          .schema( SchemaBuilder.type( "string").format( other).build())
          .build());
        });

    Optional.ofNullable( schema.getPattern())
      .flatMap( pattern -> Provider.forEcmaScript().notMatching( pattern))
      .flatMap( notMatching -> withPattern( schema, notMatching))
      .ifPresent( wrongPattern -> {
        values.add(
          VarValueDefBuilder.with( "wrongPattern")
          .type( FAILURE)
          .schema( wrongPattern)
          .build());
        });
    }

  /**
   * Returns a {@link ValueDomain} derived from the given {@link Schema}.
   */
  protected ValueDomain<?> toValueDomain( Schema schema)
    {
    ValueDomain<?> domain = null;
    if( schema != null)
      {
      domain = toConstantDomain( schema);

      if( domain == null)
        {
        switch( schema.getType())
          {
          case ARRAY:
            {
            ValueDomain<?> itemDomain = toValueDomain( schema.getItems());
            if( itemDomain == null)
              {
              itemDomain = new MultiTypeDomain( NUMBER, INTEGER, STRING);
              }

            ArrayDomain<?> arrayDomain = itemDomain.arrayOf();
            arrayDomain.setItemCount( schema.getMinItems(), schema.getMaxItems());
            arrayDomain.setItemsUnique( Optional.ofNullable( schema.getUniqueItems()).orElse( false));

            domain = arrayDomain;
            break;
            }

          case BOOLEAN:
            {
            domain = new BooleanEnum();
            break;
            }

          case INTEGER:
            {
            if( "int32".equals( schema.getFormat()))
              {
              IntegerDomain integerDomain = new IntegerDomain();
              integerDomain.setRange( integerOf( schema.getMinimum()), integerOf( schema.getMaximum()));
              integerDomain.setMultipleOf( integerOf( schema.getMultipleOf()));
              domain = integerDomain;
              }
            else
              {
              LongDomain longDomain = new LongDomain();
              longDomain.setRange( longOf( schema.getMinimum()), longOf( schema.getMaximum()));
              longDomain.setMultipleOf( longOf( schema.getMultipleOf()));
              domain = longDomain;
              }
            break;
            }

          case NUMBER:
            {
            DecimalDomain decimalDomain = new DecimalDomain();
            decimalDomain.setRange( schema.getMinimum(), schema.getMaximum());
            decimalDomain.setMultipleOf( schema.getMultipleOf());
              
            domain = decimalDomain;
            break;
            }

          case STRING:
            {
            String format = schema.getFormat();

            AbstractStringDomain stringDomain =
              "date".equals( format)?
              new DateDomain() :

              "date-time".equals( format)?
              new DateTimeDomain() :

              "uuid".equals( format)?
              new UuidDomain() :

              "email".equals( format)?
              new EmailDomain() :

              new AsciiStringDomain();

            stringDomain.setLengthRange( schema.getMinLength(), schema.getMaxLength());
            Optional.ofNullable( schema.getPattern()).ifPresent( pattern -> stringDomain.setMatching( pattern));

            domain = stringDomain;
            break;
            }

          case NULL:
            {
            domain = new NullDomain();
            break;
            }

          default:
            {
            break;
            }
          }
        }
      }
    
    return domain;
    }

  /**
   * If the given {@link Schema} defines a constant value, returns the corresponding {@link ConstantDomain}.
   * Otherwise, returns null.
   */
  @SuppressWarnings("unchecked")
  private ValueDomain<?> toConstantDomain( Schema schema)
    {
    ValueDomain<?> domain = null;

    DataValue<?> constant = Optional.ofNullable( schema.getConstant()).orElse( null);
    if( constant != null)
      {
      switch( constant.getType())
        {
        case ARRAY:
          {
          domain = new ArrayConstant( (ArrayValue<Object>) constant);
          break;
          }

        case BOOLEAN:
          {
          domain = new BooleanConstant( ((BooleanValue) constant).getValue());
          break;
          }

        case INTEGER:
          {
          domain =
            "int32".equals( constant.getFormat())
            ? new IntegerConstant( ((IntegerValue) constant).getValue())
            : new LongConstant( ((LongValue) constant).getValue());
          break;
          }

        case NUMBER:
          {
          domain = new DecimalConstant( ((DecimalValue) constant).getValue());
          break;
          }

        case STRING:
          {
          String format = constant.getFormat();
          domain =
            "date".equals( format)?
            new DateConstant( String.valueOf( constant.getValue())) :

            "date-time".equals( format)?
            new DateTimeConstant( String.valueOf( constant.getValue())) :

            "uuid".equals( format)?
            new UuidConstant( String.valueOf( constant.getValue())) :

            "email".equals( format)?
            new EmailConstant( String.valueOf( constant.getValue())) :

            new StringConstant( String.valueOf( constant.getValue()), format);

          break;
          }

        case NULL:
          {
          domain = new NullDomain();
          break;
          }

        default:
          {
          break;
          }
        }
      }

    return domain;
    }

  /**
   * Updates the given {@link Schema} to normalize property values.
   */
  protected Schema normalize( Schema schema)
    {
    if( !(schema == null || schema.getConstant() != null))
      {
      switch( schema.getType())
        {
        case ARRAY:
          {
          schema.setMaxItems(
            Optional.ofNullable( schema.getMaxItems())
            .filter( max -> max >= 0)
            .orElse( null));
          
          schema.setMinItems(
            Optional.ofNullable( schema.getMinItems())
            .filter( min -> min >= 0)
            .map( min -> Optional.ofNullable( schema.getMaxItems()).map( max -> adjustedMinOf( "Items", min, max)).orElse( min))
            .orElse( null));

          schema.setItems(
            Optional.ofNullable( schema.getItems())
            .map( items -> getContext().resultFor( "items", () -> normalize( items)))
            .orElse( null));
          break;
          }

        case INTEGER:
        case NUMBER:
          {
          normalizeNumberRange( schema);
          break;
          }

        case STRING:
          {
          schema.setPattern(
            Optional.ofNullable( schema.getPattern())
            .filter( pattern -> isPatternApplicable( schema.getFormat()))
            .filter( this::isPatternValid)
            .orElse( null));

          normalizeStringLength( schema);
          break;
          }

        default:
          {
          break;
          }
        }
      }
    
    return schema;
    }

  /**
   * Updates the given numeric {@link Schema} to define the effective minimum and maximum (inclusive) values.
   */
  private Schema normalizeNumberRange( Schema schema)
    {
    BigDecimal unit = unitOf( schema);

    schema.setMultipleOf(
      Optional.ofNullable( schema.getMultipleOf())
      .filter( m -> m.compareTo( BigDecimal.ZERO) != 0)
      .orElse( null));
      
    BigDecimal effMultipleOf =
      Optional.ofNullable( schema.getMultipleOf())
      .orElse( unit);
    
    BigDecimal effMin =
      Optional.ofNullable(
        Optional.ofNullable( schema.getExclusiveMinimum())
        .map( xm -> xm.add( effMultipleOf))
        .map( xm -> Optional.ofNullable( schema.getMinimum()).filter( m -> m.compareTo( xm) >= 0).orElse( xm))
        .orElse( schema.getMinimum()))

      .map( min -> multipleAbove( min, effMultipleOf))
      .orElse( null);


    BigDecimal effMax =
      Optional.ofNullable(
        Optional.ofNullable( schema.getExclusiveMaximum())
        .map( xm -> xm.subtract( effMultipleOf))
        .map( xm -> Optional.ofNullable( schema.getMaximum()).filter( m -> m.compareTo( xm) <= 0).orElse( xm))
        .orElse( schema.getMaximum()))

      .map( max -> multipleBelow( max, effMultipleOf))
      .orElse( null);

    schema.setMinimum(
      Optional.ofNullable( effMin)
      .map( min -> Optional.ofNullable( effMax).map( max -> adjustedMinOf( "imum", min, max)).orElse( min))
      .orElse( null));
    schema.setMaximum( effMax);

    schema.setExclusiveMinimum( null);
    schema.setExclusiveMaximum( null);

    return schema;
    }

  /**
   * Updates the given string {@link Schema} to define the effective minimum and maximum (inclusive) length.
   */
  private Schema normalizeStringLength( Schema schema)
    {
    Integer[] lengthRequired = lengthRequired( schema); 
    Integer minRequired = lengthRequired[0];
    Integer maxRequired = lengthRequired[1];

    Optional.ofNullable( minRequired)
      .flatMap( min -> Optional.ofNullable( maxRequired))
      .filter( max -> max < minRequired)
      .ifPresent( infeasible -> {
        notifyError(
          String.format( "Required length for pattern='%s' is incompatible with format='%s'", schema.getPattern(), schema.getFormat()),
          "Ignoring the pattern for this schema");
        schema.setPattern( null);
        }); 

    schema.setMaxLength(
      Optional.ofNullable( schema.getMaxLength())
      .filter( max -> max >= 0)
      .map( max -> adjustToRange( "maxLength", max, minRequired, maxRequired))
      .orElse( maxRequired));

    schema.setMinLength(
      Optional.ofNullable( schema.getMinLength())
      .filter( min -> min >= 0)
      .map( min -> adjustToRange( "minLength", min, minRequired, maxRequired))
      .map( min -> Optional.ofNullable( schema.getMaxLength()).map( max -> adjustedMinOf( "Length", min, max)).orElse( min))
      .orElse( minRequired));

    return schema;
    }

  /**
   * Returns the adjusted minimum of the given range.
   */
  private <T extends Comparable<T>> T adjustedMinOf( String description, T min, T max)
    {
    if( min.compareTo( max) > 0)
      {
      notifyError(
        String.format(
          "min%s=%s is greater than max%s=%s",
          description, min,
          description, max),

        String.format(
          "Adjusting min%s to max%s",
          description,
          description));

      min = max;
      }

    return min;
    }

  /**
   * Adjusts the given value to lie within the given range.
   */
  private <T extends Comparable<T>> T adjustToRange( String description, T value, T min, T max)
    {
    if( max != null && value.compareTo( max) > 0)
      {
      notifyError(
        String.format( "%s=%s is greater than the required maximum=%s", description, value, max),
        String.format( "Adjusting %s to the required maximum", description));

      value = max;
      }

    if( min != null && value.compareTo( min) < 0)
      {
      notifyError(
        String.format( "%s=%s is less than the required minimum=%s", description, value, min),
        String.format( "Adjusting %s to the required minimum", description));

      value = min;
      }

    return value;
    }

  /**
   * Return the largest number less than (or, if inclusive, equal to) the given value that satisfies the given (not-)multiple-of constraints.
   */
  private BigDecimal multipleBelow( BigDecimal value, BigDecimal multipleOf)
    { 
    return
      isMultipleOf( value, multipleOf)
      ? value
      : value.divide( multipleOf, 0, FLOOR).multiply( multipleOf);
    }

  /**
   * Return the smallest number greater than (or, if inclusive, equal to) the given value that satisfies the given (not-)multiple-of constraints.
   */
  private BigDecimal multipleAbove( BigDecimal value, BigDecimal multipleOf)
    {
    return
      isMultipleOf( value, multipleOf)
      ? value
      : value.divide( multipleOf, 0, CEILING).multiply( multipleOf);
    }

  /**
   * Returns true if the given value is a multiple of the given factor.
   */
  private static boolean isMultipleOf( BigDecimal value, BigDecimal factor)
    {
    return
      value.compareTo( BigDecimal.ZERO) == 0
      ||
      value.remainder( factor).compareTo( BigDecimal.ZERO) == 0;
    }

  /**
   * Returns true if pattern matching is supported for the given string format.
   */
  private boolean isPatternApplicable( String format)
    {
    boolean applicable = !isPatternedFormat( format);
    if( !applicable)
      {
      notifyWarning( String.format( "Pattern matching not supported for strings with format=%s. Ignoring the pattern for this schema", format));
      }
    return applicable;
    }

  /**
   * Returns true if the given pattern is valid
   */
  private boolean isPatternValid( String pattern)
    {
    try
      {
      Provider.forEcmaScript().matching( pattern);
      return true;
      }
    catch( IllegalArgumentException e)
      {
      notifyError( String.format( "Invalid pattern: %s", e.getMessage()), "Ignoring the pattern for this schema");
      return false;
      }
    }

  /**
   * Returns a version of the given schema that will accept strings produced by the given generator.
   * Returns {@link Optional.empty} if this schema is not compatible with this generator.
   */
  private Optional<Schema> withPattern( Schema schema, RegExpGen patternGen)
    {
    Integer minPattern = minPatternMatch( patternGen);
    Integer maxPattern = maxPatternMatch( patternGen);
    
    Integer minLength =
      Optional.ofNullable( schema.getMinLength())
      .map( mnl -> Math.max( mnl, minPattern))
      .orElse( minPattern);

    Integer maxLength =
      Optional.ofNullable( schema.getMaxLength())
      .map( mxl -> Optional.ofNullable( maxPattern).map( mxp -> Math.min( mxl, mxp)).orElse( mxl))
      .orElse( maxPattern);

    return
      Optional.of( schema)
      .filter( s -> !(maxLength != null && maxLength < minLength))
      .map( s -> {
        return
          SchemaBuilder.with( s)
          .minLength( minLength)
          .maxLength( maxLength)
          .pattern( patternGen.getSource())
          .build();
        });      
    }

  /**
   * Returns the maximum length required for pattern matches.
   */
  private Integer maxPatternMatch( Schema schema)
    {
    return
      Optional.ofNullable( schema.getPattern())
      .flatMap( pattern -> Optional.ofNullable( maxPatternMatch( Provider.forEcmaScript().matching( pattern))))
      .orElse( null);
    }

  /**
   * Returns the maximum length required for the given pattern generator.
   */
  private Integer maxPatternMatch( RegExpGen patternGen)
    {
    return bounded( patternGen.getMaxLength()).orElse( null);
    }

  /**
   * Returns the minimum length required for pattern matches.
   */
  private Integer minPatternMatch( Schema schema)
    {
    return
      Optional.ofNullable( schema.getPattern())
      .map( pattern -> minPatternMatch( Provider.forEcmaScript().matching( pattern)))
      .orElse( null);
    }

  /**
   * Returns the minimum length required for the given pattern generator.
   */
  private Integer minPatternMatch( RegExpGen patternGen)
    {
    return patternGen.getMinLength();
    }

  /**
   * Returns an array of the form [minLength, maxLength] that defines the string length required by the given schema.
   */
  private Integer[] lengthRequired( Schema schema)
    {
    Integer minRequired = minLengthRequired( schema);
    Integer maxRequired = maxLengthRequired( schema);
    if( minRequired != null && maxRequired != null && minRequired > maxRequired)
      {
      notifyError(
        String.format( "Required length for pattern='%s' is incompatible with format='%s'", schema.getPattern(), schema.getFormat()),
        "Ignoring the pattern for this schema");

      schema.setPattern( null);
      minRequired = stringFormatMin( schema.getFormat());
      maxRequired = stringFormatMax( schema.getFormat());
      }

    return new Integer[]{ minRequired, maxRequired};
    }

  /**
   * Returns the minimum string length required by the given schema.
   */
  private Integer minLengthRequired( Schema schema)
    {
    Integer formatMin = stringFormatMin( schema.getFormat());
    Integer patternMin = minPatternMatch( schema);

    return
      Optional.ofNullable( formatMin)
      .map( fm -> Optional.ofNullable( patternMin).map( pm -> Math.max( fm, pm)).orElse( fm))
      .orElse( patternMin);
    }

  /**
   * Returns the maximum string length required by the given schema.
   */
  private Integer maxLengthRequired( Schema schema)
    {
    Integer formatMax = stringFormatMax( schema.getFormat());
    Integer patternMax = maxPatternMatch( schema);

    return
      Optional.ofNullable( formatMax)
      .map( fm -> Optional.ofNullable( patternMax).map( pm -> Math.min( fm, pm)).orElse( fm))
      .orElse( patternMax);
    }

  /**
   * Returns the unit value for the number range defined by the given schema.
   */
  private BigDecimal unitOf( Schema schema)
    {
    return
      new BigDecimal(
        BigInteger.ONE,
        Stream.of(
          Optional.ofNullable( schema.getMultipleOf()).map( BigDecimal::scale).orElse( 0),
          Optional.ofNullable( schema.getMinimum()).map( BigDecimal::scale).orElse( 0),
          Optional.ofNullable( schema.getMaximum()).map( BigDecimal::scale).orElse( 0),
          Optional.ofNullable( schema.getExclusiveMinimum()).map( BigDecimal::scale).orElse( 0),
          Optional.ofNullable( schema.getExclusiveMaximum()).map( BigDecimal::scale).orElse( 0))
        .max( Integer::compareTo)
        .orElse( 0));
    }
  }
