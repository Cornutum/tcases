//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.*;
import static org.cornutum.tcases.resolve.DataValue.Type.*;
import static org.cornutum.tcases.resolve.DataValues.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import org.cornutum.regexpgen.RegExpGen;
import org.cornutum.regexpgen.js.Parser;
import static org.cornutum.regexpgen.Bounds.bounded;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Optional;
import java.util.Random;
import java.util.stream.Stream;
import static java.math.RoundingMode.*;

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
    this( ResolverContext.builder( new Random()).notifier( TestCaseConditionNotifier.log()).build());
    }
  
  /**
   * Creates a new TestCaseSchemaResolver instance.
   */
  public TestCaseSchemaResolver( ResolverContext context)
    {
    context_ = context;
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
    getContext().doFor(
      inputDef.getName(),
      () ->
      toStream( new VarDefIterator( inputDef))
      .map( this::addValueDefs)
      .forEach( varDef -> prepareValueDefs( varDef)));

    return inputDef;
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
    return varDef;
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
  protected ValueDomain<?> toConstantDomain( Schema schema)
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
    if( schema != null)
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
            .map( items -> getContext().resultFor( "items", () -> normalize( new Schema( items))))
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
    BigDecimal unit =
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

    BigDecimal effMultipleOf =
      Optional.ofNullable( schema.getMultipleOf())
      .filter( m -> m.compareTo( BigDecimal.ZERO) != 0)
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
    String format = schema.getFormat();
    Integer formatMin = stringFormatMin( format);
    Integer formatMax = stringFormatMax( format);

    Integer patternMin = minPatternMatch( schema);
    Integer patternMax = maxPatternMatch( schema);

    Integer minRequired =
      Optional.ofNullable( formatMin)
      .map( fm -> Optional.ofNullable( patternMin).map( pm -> Math.max( fm, pm)).orElse( fm))
      .orElse( patternMin);

    Integer maxRequired =
      Optional.ofNullable( formatMax)
      .map( fm -> Optional.ofNullable( patternMax).map( pm -> Math.min( fm, pm)).orElse( fm))
      .orElse( patternMax);

    Optional.ofNullable( minRequired)
      .flatMap( min -> Optional.ofNullable( maxRequired))
      .filter( max -> max < minRequired)
      .ifPresent( infeasible -> {
        notifyError(
          String.format( "Required length for pattern='%s'is incompatible with format='%s'", schema.getPattern(), format),
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
   * Returns the maximum length required for pattern matches.
   */
  private Integer maxPatternMatch( Schema schema)
    {
    return
      Optional.ofNullable( schema.getPattern())
      .map( pattern -> Parser.parseRegExp( pattern))
      .map( RegExpGen::getMaxLength)
      .flatMap( matchMax -> bounded( matchMax))
      .orElse( null);
    }

  /**
   * Returns the minimum length required for pattern matches.
   */
  private Integer minPatternMatch( Schema schema)
    {
    return
      Optional.ofNullable( schema.getPattern())
      .map( pattern -> Parser.parseRegExp( pattern))
      .map( RegExpGen::getMinLength)
      .orElse( null);
    }  

  /**
   * Returns the context for this resolver.
   */
  private ResolverContext getContext()
    {
    return context_;
    }

  /**
   * Reports an error that would have resulted in an inconsistent or infeasible test case.
   *
   * @param reason  A description of the problem
   * @param resolution  A description of how the problem was resolved
   */
  private void notifyError( String reason, String resolution)
    {
    getContext().error( reason, resolution);
    }

  private final ResolverContext context_;
  }
