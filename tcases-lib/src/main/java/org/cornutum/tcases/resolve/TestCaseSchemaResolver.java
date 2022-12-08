//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.*;
import static org.cornutum.tcases.VarValueDef.Type.FAILURE;
import static org.cornutum.tcases.resolve.AbstractValueDomain.withFormat;
import static org.cornutum.tcases.resolve.DataValue.Type.*;
import static org.cornutum.tcases.resolve.DataValues.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.cornutum.regexpgen.RegExpGen;
import org.cornutum.regexpgen.js.Provider;
import java.math.BigDecimal;
import java.util.Optional;
import java.util.stream.Stream;
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
    schemas_ = new Schemas( getContext());
    }
  
  /**
   * Creates a new TestCaseSchemaResolver instance.
   */
  public TestCaseSchemaResolver( ResolverContext context)
    {
    super( context);
    schemas_ = new Schemas( context);
    }

  /**
   * Returns a binding that resolves the value of the given input variable.
   */
  @Override
  protected VarBinding resolveBinding( VarDef varDef, VarValueDef valueDef)
    {
    VarBinding binding = VarBinding.create( varDef, valueDef);
    
    Optional.ofNullable( valueDef.getDomain())
      .ifPresent( domain -> {
        binding.setSource( binding.getValue());
        binding.setValue( selectValue( domain));
        binding.setAnnotation( "format", domain.getFormat());
        });

    return binding;
    }

  /**
   * Returns a new value from the given domain.
   */
  private Object selectValue( ValueDomain<?> domain)
    {
    try
      {
      return valueObject( domain.select( getContext()));
      }
    catch( ResolverSkipException skip)
      {
      throw skip;
      }
    catch( Exception e)
      {
      throw new ResolverException( String.format( "Can't get value from %s", domain), e);
      }
    } 

  /**
   * Prepare for resolution of input value definitions
   */
  @Override
  protected FunctionInputDef prepareValueDefs( FunctionInputDef inputDef)
    {
    FunctionInputDef prepared = FunctionInputDefBuilder.with( inputDef).build();
    
    doFor(
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
    doFor(
      varDef.getPathName(),
      () -> 
      toStream( varDef.getValues())
      .forEach( valueDef -> {
        prepareValueDef(
          valueDef,
          Optional.ofNullable( valueDef.getSchema())
          .map( valueSchema -> Schemas.merge( varDef.getSchema(), valueSchema))
          .orElse( null));
        }));
    }
  
  /**
   * Prepare for resolution of the give value definition.
   */
  private void prepareValueDef( VarValueDef valueDef, Schema schema)
    {
    valueDef.setDomain(
      resultFor(
        String.valueOf( valueDef.getName()),
        () -> toValueDomain( schemas_.normalize( schema))));
    }

  /**
   * If necessary, update the given variable with value definitions generated from the variable schema.
   */
  private VarDef addValueDefs( VarDef varDef)
    {
    if( !varDef.getValues().hasNext())
      {
      doFor(
        varDef.getPathName(),
        () -> {

        valuesForSchema(
          Optional.ofNullable( varDef.getSchema())
          .orElseThrow( () -> new IllegalStateException( "No schema or values defined for this variable")))
          
          .forEach( value -> varDef.addValue( value));
        });

      varDef.setSchema( null);
      }

    return varDef;
    }

  /**
   * Returns value definitions derived from the given {@link Schema}.
   */
  protected Stream<VarValueDef> valuesForSchema( Schema schema)
    {
    schemas_.normalize( schema);
    
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

    Optional.ofNullable( schema.getItems())
      .ifPresent( items -> {
        values.add(
          VarValueDefBuilder.with( "wrongItems")
          .type( FAILURE)
          .schema(
            SchemaBuilder.with( schema)
            .items( Schemas.not( items))
            .build())
          .build());
        });
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
        .schema( SchemaBuilder.type( "number").constant( BigDecimal.ZERO).format( schema.getFormat()).build())
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
        BigDecimal unit = Schemas.unitOf( schema);
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
          .schema(
            SchemaBuilder.type( "number")
            .constant( notMultiple)
            .format( schema.getFormat())
            .build())
          .build());
        });

    }

  /**
   * Adds value definitions derived from a string {@link Schema} to the given stream.
   */
  private void addStringValues( Stream.Builder<VarValueDef> values, Schema schema)
    {
    Integer minRequired = Schemas.minLengthRequired( schema);
    Integer maxRequired = Schemas.maxLengthRequired( schema);

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
              itemDomain = withFormat( new MultiTypeDomain( NUMBER, INTEGER, STRING), schema.getFormat());
              }

            ArrayDomain<?> arrayDomain = itemDomain.arrayOf();
            arrayDomain.setItemCount( schema.getMinItems(), schema.getMaxItems());
            arrayDomain.setItemsUnique( Optional.ofNullable( schema.getUniqueItems()).orElse( false));

            domain = withFormat( arrayDomain, schema.getFormat());
            break;
            }

          case BOOLEAN:
            {
            domain = withFormat( new BooleanEnum(), schema.getFormat());
            break;
            }

          case INTEGER:
            {
            if( "int32".equals( schema.getFormat()))
              {
              IntegerDomain integerDomain = new IntegerDomain();
              integerDomain.setRange( integerOf( schema.getMinimum()), integerOf( schema.getMaximum()));
              integerDomain.setMultipleOf( integerOf( schema.getMultipleOf()));
              domain = withFormat( integerDomain, schema.getFormat());
              }
            else
              {
              LongDomain longDomain = new LongDomain();
              longDomain.setRange( longOf( schema.getMinimum()), longOf( schema.getMaximum()));
              longDomain.setMultipleOf( longOf( schema.getMultipleOf()));
              domain = withFormat( longDomain, schema.getFormat());
              }
            break;
            }

          case NUMBER:
            {
            DecimalDomain decimalDomain = new DecimalDomain();
            decimalDomain.setRange( schema.getMinimum(), schema.getMaximum());
            decimalDomain.setMultipleOf( schema.getMultipleOf());
              
            domain = withFormat( decimalDomain, schema.getFormat());
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

              withFormat( new AsciiStringDomain(), schema.getFormat());

            stringDomain.setLengthRange( schema.getMinLength(), schema.getMaxLength());
            Optional.ofNullable( schema.getPattern()).ifPresent( pattern -> stringDomain.setMatching( pattern));

            domain = stringDomain;
            break;
            }

          case NULL:
            {
            domain = withFormat( new NullDomain(), schema.getFormat());
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
          domain = withFormat( new ArrayConstant( (ArrayValue<Object>) constant), schema.getFormat());
          break;
          }

        case BOOLEAN:
          {
          domain = withFormat( new BooleanConstant( ((BooleanValue) constant).getValue()), schema.getFormat());
          break;
          }

        case INTEGER:
          {
          domain =
            "int32".equals( constant.getFormat())
            ? new IntegerConstant( ((IntegerValue) constant).getValue())
            : withFormat( new LongConstant( ((LongValue) constant).getValue()), schema.getFormat());
          break;
          }

        case NUMBER:
          {
          domain = withFormat( new DecimalConstant( ((DecimalValue) constant).getValue()), schema.getFormat());
          break;
          }

        case STRING:
          {
          String format = constant.getFormat();
          String constantString = String.valueOf( constant.getValue());
          domain =
            "date".equals( format)?
            new DateConstant( constantString) :

            "date-time".equals( format)?
            new DateTimeConstant( constantString) :

            "uuid".equals( format)?
            new UuidConstant( constantString) :

            "email".equals( format)?
            new EmailConstant( constantString) :

            new StringConstant( constantString, format);

          break;
          }

        case NULL:
          {
          domain = withFormat( new NullDomain(), schema.getFormat());
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
   * Returns a version of the given schema that will accept strings produced by the given generator.
   * Returns {@link Optional.empty} if this schema is not compatible with this generator.
   */
  private Optional<Schema> withPattern( Schema schema, RegExpGen patternGen)
    {
    Integer minPattern = Schemas.minPatternMatch( patternGen);
    Integer maxPattern = Schemas.maxPatternMatch( patternGen);
    
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

  private final Schemas schemas_;
  }
