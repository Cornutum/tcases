//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.conditions.*;

import static org.cornutum.tcases.VarValueDef.Type.*;
import static org.cornutum.tcases.io.SystemInputDoc.*;
import static org.cornutum.tcases.util.CollectionUtils.toStream;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Optional;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

/**
 * Writes a {@link SystemInputDef} in the form of an XML document.
 *
 */
public class SystemInputDocWriter extends AbstractSystemInputWriter
  {
  /**
   * Creates a new SystemInputDocWriter object that writes to standard output.
   */
  public SystemInputDocWriter()
    {
    super();
    }
  
  /**
   * Creates a new SystemInputDocWriter object that writes to the given stream.
   */
  public SystemInputDocWriter( OutputStream stream)
    {
    super( stream);
    }
  
  /**
   * Creates a new SystemInputDocWriter object that writes to the given stream.
   */
  public SystemInputDocWriter( Writer writer)
    {
    super( writer);
    }

  /**
   * Writes the given system test definition the form of an XML document.
   */
  public void write( SystemInputDef systemInput)
    {
    xmlWriter_.writeDeclaration();

    xmlWriter_
      .element( SYSTEM_TAG)
      .attribute( NAME_ATR, systemInput.getName())
      .content( () ->
        {
        writeAnnotations( systemInput);
        toStream( systemInput.getFunctionInputDefs()).forEach( this::writeFunction);
        })
      .write();
    }

  /**
   * Writes the given function input definition.
   */
  protected void writeFunction( FunctionInputDef function)
    {
    xmlWriter_
      .element( FUNCTION_TAG)
      .attribute( NAME_ATR, function.getName())
      .content( () ->
        {
        writeAnnotations( function);
        for( String varType : function.getVarTypes())
          {
          writeInputs(
            varType,
            toStream( function.getVarDefs())
            .filter( varDef -> varDef.getType().equals( varType))
            .sorted());
          }
        })
      .write();
    }

  /**
   * Writes the given input variable list.
   */
  protected void writeInputs( String varType, Stream<IVarDef> varDefs)
    {
    xmlWriter_
      .element( INPUT_TAG)
      .attribute( TYPE_ATR, varType)
      .content( () -> varDefs.forEach( this::writeVarDef))
      .write();
    }

  /**
   * Writes the given variable definition.
   */
  protected void writeVarDef( IVarDef varDef)
    {
    Stream<VarValueDef> values =
      varDef.getValues() == null
      ? null
      : toStream( varDef.getValues());

    Stream<IVarDef> members = 
      varDef.getMembers() == null
      ? null
      : toStream( varDef.getMembers());

    String varTag =
      members == null
      ? VAR_TAG
      : VARSET_TAG;

    ConditionWriter conditionWriter = new ConditionWriter( varDef.getCondition());

    xmlWriter_
      .element( varTag)
      .attribute( NAME_ATR, varDef.getName())
      .attributeIf( WHEN_ATR, conditionWriter.getWhenAttribute())
      .attributeIf( WHENNOT_ATR, conditionWriter.getWhenNotAttribute())
      .content( () ->
        {
        writeAnnotations( varDef);
        conditionWriter.writeWhenElement();
        if( varTag.equals( VAR_TAG))
          {
          values.forEach( this::writeValue);
          }
        else
          {
          members.forEach( this::writeVarDef);
          }
        })
      .write();
    }

  /**
   * Writes the given variable input value definition.
   */
  protected void writeValue( VarValueDef value)
    {
    ConditionWriter conditionWriter = new ConditionWriter( value.getCondition());

    xmlWriter_
      .element( VALUE_TAG)
      .attribute( NAME_ATR, String.valueOf( value.getName()))
      .attributeIf( value.getType() == FAILURE, FAILURE_ATR, "true")
      .attributeIf( value.getType() == ONCE, ONCE_ATR, "true")
      .attributeIf( WHEN_ATR, conditionWriter.getWhenAttribute())
      .attributeIf( WHENNOT_ATR, conditionWriter.getWhenNotAttribute())
      .attributeIf( PROPERTY_ATR, propertyList( toStream( value.getProperties())))
      .contentIf(
        value.getAnnotationCount() > 0 || conditionWriter.hasWhenElement(),
        () -> { writeAnnotations( value); conditionWriter.writeWhenElement(); })
      .write();
    }

  /**
   * Writes the given annotation definitions.
   */
  protected void writeAnnotations( IAnnotated annotated)
    {
    toStream( annotated.getAnnotations())
      .sorted()
      .forEach( annotation -> {
        xmlWriter_
          .element( HAS_TAG)
          .attribute( NAME_ATR, annotation)
          .attribute( VALUE_ATR, annotated.getAnnotation( annotation))
          .write();
        });
    }

  private Optional<String> propertyList( Stream<String> properties)
    {
    return
      properties
      .distinct()
      .sorted()
      .reduce( (list, property) -> list + ", " + property);
    } 

  /**
   * Flushes the writer.
   */
  public void flush()
    {
    getXmlWriter().flush();
    }

  /**
   * Closes the writer.
   */
  public void close() throws IOException
    {
    getXmlWriter().close();
    }

  /**
   * Changes the output stream for this writer.
   */
  protected void setWriter( Writer writer)
    {
    setXmlWriter( new XmlWriter( writer));
    }

  /**
   * Changes the XmlWriter for this writer.
   */
  private void setXmlWriter( XmlWriter xmlWriter)
    {
    xmlWriter_ = xmlWriter;
    }

  /**
   * Returns the XmlWriter for this writer.
   */
  private XmlWriter getXmlWriter()
    {
    return xmlWriter_;
    }

  private class ConditionWriter implements IConditionVisitor
    {
    public ConditionWriter( ICondition condition)
      {
      condition_ = condition;

      Optional<AllOf> attributes = asAttributes( condition);
      when_ =
        attributes.isPresent()
        ? attributes.flatMap( this::whenAttributeOf).get()
        : asWhenCondition( condition).orElse( null);
      whenNot_ =
        attributes.isPresent()
        ? attributes.flatMap( this::whenNotAttributeOf).get()
        : asWhenNotCondition( condition).orElse( null);
      }

    public boolean hasWhenElement()
      {
      return condition_ != null && when_ == null && whenNot_ == null;
      }

    public void writeWhenElement()
      {
      if( hasWhenElement())
        {
        xmlWriter_
          .element( WHEN_TAG)
          .content( () -> condition_.accept( this))
          .write();
        }
      }

    public Optional<String> getWhenAttribute()
      {
      return
        when_ != null
        ? propertyList( toStream( when_.getProperties()))
        : Optional.empty();
      }

    public Optional<String> getWhenNotAttribute()
      {
      return
        whenNot_ != null
        ? propertyList( toStream( whenNot_.getProperties()))
        : Optional.empty();
      }
    
    public void visit( AllOf condition)
      {
      xmlWriter_
        .element( ALLOF_TAG)
        .attributeIf( PROPERTY_ATR, propertiesOf( condition, ContainsAll.class))
        .content( () ->
                visit( withoutPropertiesOf( condition, ContainsAll.class)))
        .write();
      }
  
    public void visit( AnyOf condition)
      {
      xmlWriter_
        .element( ANYOF_TAG)
        .attributeIf( PROPERTY_ATR, propertiesOf( condition, ContainsAny.class))
        .content( () ->
                visit( withoutPropertiesOf( condition, ContainsAny.class)))
        .write();
      }
  
    public void visit( ContainsAll condition)
      {
      xmlWriter_
        .element( ALLOF_TAG)
        .attributeIf( PROPERTY_ATR, propertyList( toStream( condition.getProperties())))
        .write();
      }
  
    public void visit( ContainsAny condition)
      {
      xmlWriter_
        .element( ANYOF_TAG)
        .attributeIf( PROPERTY_ATR, propertyList( toStream( condition.getProperties())))
        .write();
      }
  
    public void visit( IConjunct condition)
      {
      throw new UnsupportedOperationException( "Unexpected IConjunct in SystemInputDef");
      }
  
    public void visit( Not condition)
      {
      xmlWriter_
        .element( NOT_TAG)
        .attributeIf( PROPERTY_ATR, propertiesOf( condition, ContainsAny.class))
        .content( () ->
                visit( withoutPropertiesOf( condition, ContainsAny.class)))
        .write();
      }

    public void visit( AssertLess condition)
      {
      xmlWriter_
        .element( LESSTHAN_TAG)
        .attribute( PROPERTY_ATR, condition.getProperty())
        .attribute( MAX_ATR, String.valueOf( condition.getBound()))
        .write();
      }

    public void visit( AssertMore condition)
      {
      xmlWriter_
        .element( MORETHAN_TAG)
        .attribute( PROPERTY_ATR, condition.getProperty())
        .attribute( MIN_ATR, String.valueOf( condition.getBound()))
        .write();
      }

    public void visit( AssertNotLess condition)
      {
      xmlWriter_
        .element( NOTLESSTHAN_TAG)
        .attribute( PROPERTY_ATR, condition.getProperty())
        .attribute( MIN_ATR, String.valueOf( condition.getBound()))
        .write();
      }

    public void visit( AssertNotMore condition)
      {
      xmlWriter_
        .element( NOTMORETHAN_TAG)
        .attribute( PROPERTY_ATR, condition.getProperty())
        .attribute( MAX_ATR, String.valueOf( condition.getBound()))
        .write();
      }

    public void visit( Between condition)
      {
      BoundedAssertion min = condition.getMin();
      BoundedAssertion max = condition.getMax();

      xmlWriter_
        .element( BETWEEN_TAG)
        .attribute( PROPERTY_ATR, min.getProperty())
        .attribute( min.isExclusive()? EXCLMIN_ATR : MIN_ATR, String.valueOf( min.getBound()))
        .attribute( max.isExclusive()? EXCLMAX_ATR : MAX_ATR, String.valueOf( max.getBound()))
        .write();
      }

    public void visit( Equals condition)
      {
      BoundedAssertion min = condition.getMin();

      xmlWriter_
        .element( EQUALS_TAG)
        .attribute( PROPERTY_ATR, min.getProperty())
        .attribute( COUNT_ATR, String.valueOf( min.getBound()))
        .write();
      }
  
    private void visit( Stream<ICondition> conditions)
      {
      conditions.forEach( condition -> condition.accept( this));
      }

    private <T extends PropertyExpr> Optional<String> propertiesOf( ConditionSet condition, Class<T> propExprClass)
      {
      return
        propertyList(
          toStream( condition.getConditions())
          .filter( c -> c.getClass().equals( propExprClass))
          .flatMap( e -> toStream( ((PropertyExpr) e).getProperties())));
      }

    private <T extends PropertyExpr> Stream<ICondition> withoutPropertiesOf( ConditionSet condition, Class<T> propExprClass)
      {
      return
        toStream( condition.getConditions())
        .filter( c -> !c.getClass().equals( propExprClass));
      }

    private Optional<AllOf> asAttributes( ICondition condition)
      {
      AllOf allOf =
        condition != null && condition.getClass().equals( AllOf.class)
        ? (AllOf) condition
        : null;

      return
        // Is this an AllOf condition...
        Optional.ofNullable( allOf)

        // ... containing only a "when" condition and a "whenNot" condition?
        .filter( conditionSet -> whenAttributeOf( conditionSet).isPresent() && whenNotAttributeOf( conditionSet).isPresent());
      }
    
    private Optional<ContainsAll> asWhenCondition( ICondition condition)
      {
      return
        condition != null && condition.getClass().equals( ContainsAll.class)
        ? Optional.of( (ContainsAll) condition)
        : Optional.empty();
      }

    private Optional<ContainsAny> asWhenNotCondition( ICondition condition)
      {
      Not not =
        condition != null && condition.getClass().equals( Not.class)
        ? (Not) condition
        : null;
      
      return
        // Is this a Not condition...
        Optional.ofNullable( not)

        // ... with exactly 1 member...
        .map( conditionSet -> toStream( conditionSet.getConditions()).collect( toList()))
        .filter( conditions -> conditions.size() == 1)

        // ... which is a ContainsAny condition?
        .map( conditions -> conditions.get(0))
        .flatMap( c -> Optional.ofNullable( c.getClass().equals( ContainsAny.class)? (ContainsAny) c : null));
      }

    private Optional<ContainsAll> whenAttributeOf( AllOf allOf)
      {
      return
        // If this is a condition set with exactly 2 members...
        Optional.of( toStream( allOf.getConditions()).collect( toList()))
        .filter( conditions -> conditions.size() == 2)

        // ... containing exactly 1 "when" condition...
        .map( conditions -> conditions.stream().map( this::asWhenCondition).filter( Optional::isPresent).collect( toList()))
        .filter( whens -> whens.size() == 1)

        // ... then return the "when" condition
        .map( whens -> whens.get(0).get());
      }

    private Optional<ContainsAny> whenNotAttributeOf( AllOf allOf)
      {
      return
        // If this is a condition set with exactly 2 members...
        Optional.of( toStream( allOf.getConditions()).collect( toList()))
        .filter( conditions -> conditions.size() == 2)

        // ... containing exactly 1 "whenNot" condition...
        .map( conditions -> conditions.stream().map( this::asWhenNotCondition).filter( Optional::isPresent).collect( toList()))
        .filter( whenNots -> whenNots.size() == 1)

        // ... then return the "whenNot" condition
        .map( whenNots -> whenNots.get(0).get());
      }

    private ICondition condition_;
    private ContainsAll when_;
    private ContainsAny whenNot_;
    }
  
  private XmlWriter xmlWriter_;
  }
