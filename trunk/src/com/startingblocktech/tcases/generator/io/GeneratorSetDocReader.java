//////////////////////////////////////////////////////////////////////////////
// 
//               Copyright 2010, Starting Block Technologies
//                        www.startingblocktech.com
//
//////////////////////////////////////////////////////////////////////////////

package com.startingblocktech.tcases.generator.io;

import com.startingblocktech.tcases.generator.*;
import static com.startingblocktech.tcases.DefUtils.*;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import org.apache.commons.lang.StringUtils;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * An {@link IGeneratorSetSource} that reads from an XML document.
 *
 * @version $Revision$, $Date$
 */
public class GeneratorSetDocReader extends DefaultHandler implements IGeneratorSetSource
  {
  /**
   * The base class for element handlers used by this parser.
   *
   * @version $Revision$, $Date$
   */
  protected class ElementHandler extends DefaultHandler
    {
    /**
     * Returns the value of the given attribute. Throws a SAXException if the attribute is not defined.
     */
    public String requireAttribute( Attributes attributes, String attributeName, String attributeValue) throws SAXException
      {
      if( attributeValue == null)
        {
        throw new SAXParseException( "No \"" + attributeName + "\" attribute specified", getDocumentLocator()); 
        }

      return attributeValue;
      }

    /**
     * Returns the value of the given attribute. Throws a SAXException if the attribute is undefined or empty.
     */
    public String requireAttribute( Attributes attributes, String attributeName) throws SAXException
      {
      return requireAttribute( attributes, attributeName, getAttribute( attributes, attributeName));
      }

    /**
     * Returns the value of the given identifier attribute. Throws a SAXException if the attribute is undefined, empty, or invalid.
     */
    public String requireIdentifier( Attributes attributes, String attributeName) throws SAXException
      {
      String id = requireAttribute( attributes, attributeName);

      try
        {
        assertIdentifier( id);
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Invalid \"" + attributeName + "\" attribute", getDocumentLocator(), e); 
        }

      return id;
      }
      
    /**
     * Returns the value of the given attribute as a Long. Returns null if the attribute value is undefined or empty.
     */
    public Long getLongAttribute( Attributes attributes, String attributeName) throws SAXParseException
      {
      String value = getAttribute( attributes, attributeName);
      Long longValue = null;
      if( value != null)
        {
        try
          {
          longValue = Long.valueOf( value);
          }
        catch( NumberFormatException e)
          {
          throw
            new SAXParseException
            ( "Invalid \"" + attributeName + "\" attribute: \"" + value + "\" is not a number",
              getDocumentLocator()); 
          }
        }

      return longValue;
      }
      
    /**
     * Returns the value of the given attribute. Returns null if the attribute value is undefined or empty.
     */
    public String getAttribute( Attributes attributes, String attributeName)
      {
      return StringUtils.trimToNull( attributes.getValue( attributeName));
      }

    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return false;
      }
      
    /**
     * Changes the parent ElementHandler.
     */
    public void setParent( ElementHandler parent)
      {
      parent_ = parent;
      }

    /**
     * Returns the parent ElementHandler.
     */
    public ElementHandler getParent()
      {
      return parent_;
      }

    private ElementHandler parent_;
    }
  
  /**
   * Handles Generators elements.
   *
   * @version $Revision$, $Date$
   */
  protected class GeneratorsHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return TUPLEGENERATOR_TAG.equals( memberQname);
      }
    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setGeneratorSet( new GeneratorSet());
      }

    /**
     * Changes the {@link GeneratorSet} represented by this element.
     */
    private void setGeneratorSet( GeneratorSet generatorSet)
      {
      generatorSet_ = generatorSet;
      }

    /**
     * Returns the {@link GeneratorSet} represented by this element.
     */
    public GeneratorSet getGeneratorSet()
      {
      return generatorSet_;
      }
    }
  
  /**
   * Handles TupleGenerator elements.
   *
   * @version $Revision$, $Date$
   */
  protected class TupleGeneratorHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return COMBINE_TAG.equals( memberQname);
      }
    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      TupleGenerator tupleGenerator = new TupleGenerator();
      setTupleGenerator( tupleGenerator);
      tupleGenerator.setRandomSeed( getLongAttribute( attributes, SEED_ATR));

      Long defaultTupleSize = getLongAttribute( attributes, TUPLES_ATR);
      if( defaultTupleSize != null)
        {
        tupleGenerator.setDefaultTupleSize( defaultTupleSize.intValue());
        }

      String functionName = getAttribute( attributes, FUNCTION_ATR);
      if( functionName == null)
        {
        functionName = GeneratorSet.ALL;
        }
      else if( !functionName.equals( GeneratorSet.ALL))
        {
        try
          {
          assertIdentifier( functionName);
          }
        catch( Exception e)
          {
          throw new SAXParseException( "Invalid \"" + FUNCTION_ATR + "\" attribute", getDocumentLocator(), e); 
          }
        }

      try
        {
        GeneratorsHandler parent = (GeneratorsHandler) getParent();
        parent.getGeneratorSet().addGenerator( functionName, tupleGenerator);
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Invalid \"" + FUNCTION_ATR + "\" attribute", getDocumentLocator(), e); 
        }
      }

    /**
     * Changes the {@link TupleGenerator} represented by this element.
     */
    private void setTupleGenerator( TupleGenerator tupleGenerator)
      {
      tupleGenerator_ = tupleGenerator;
      }

    /**
     * Returns the {@link TupleGenerator} represented by this element.
     */
    public TupleGenerator getTupleGenerator()
      {
      return tupleGenerator_;
      }

    private TupleGenerator tupleGenerator_;
    }
  
  /**
   * Handles Combine elements.
   *
   * @version $Revision$, $Date$
   */
  protected class CombineHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return
        INCLUDE_TAG.equals( memberQname)
        || EXCLUDE_TAG.equals( memberQname);
      }
    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      TupleCombiner tupleCombiner = new TupleCombiner();
      setTupleCombiner( tupleCombiner);

      Long tupleSize = getLongAttribute( attributes, TUPLES_ATR);
      if( tupleSize == null)
        {
        TupleGeneratorHandler parent = (TupleGeneratorHandler) getParent();
        tupleSize = new Long( parent.getTupleGenerator().getDefaultTupleSize());
        }
      tupleCombiner.setTupleSize( tupleSize.intValue());
      }

    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      TupleCombiner tupleCombiner = getTupleCombiner();

      if( tupleCombiner.isEmpty())
        {
        tupleCombiner.addIncludedVar( "**");
        }
      
      TupleGeneratorHandler parent = (TupleGeneratorHandler) getParent();
      parent.getTupleGenerator().addCombiner( tupleCombiner);
      }

    /**
     * Changes the {@link TupleCombiner} represented by this element.
     */
    private void setTupleCombiner( TupleCombiner tupleCombiner)
      {
      tupleCombiner_ = tupleCombiner;
      }

    /**
     * Returns the {@link TupleCombiner} represented by this element.
     */
    public TupleCombiner getTupleCombiner()
      {
      return tupleCombiner_;
      }

    private TupleCombiner tupleCombiner_;
    }
  
  /**
   * Handles Include elements.
   *
   * @version $Revision$, $Date$
   */
  protected class IncludeHandler extends ElementHandler
    {
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      CombineHandler parent = (CombineHandler) getParent();
      try
        {
        parent.getTupleCombiner().addIncludedVar( requireAttribute( attributes, VAR_ATR));
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Invalid \"" + VAR_ATR + "\" attribute", getDocumentLocator(), e); 
        }
      }
    }
  
  /**
   * Handles Exclude elements.
   *
   * @version $Revision$, $Date$
   */
  protected class ExcludeHandler extends ElementHandler
    {
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      CombineHandler parent = (CombineHandler) getParent();
      try
        {
        parent.getTupleCombiner().addExcludedVar( requireAttribute( attributes, VAR_ATR));
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Invalid \"" + VAR_ATR + "\" attribute", getDocumentLocator(), e); 
        }
      }
    }
  
  /**
   * Creates a new GeneratorSetDocReader object.
   */
  public GeneratorSetDocReader()
    {
    this( System.in);
    }
  
  /**
   * Creates a new GeneratorSetDocReader object.
   */
  public GeneratorSetDocReader( InputStream stream)
    {
    setInputStream( stream);
    }

  /**
   * Returns a {@link GeneratorSet} instance.
   */
  public IGeneratorSet getGeneratorSet()
    {
    SAXParser parser;
    try
      {
      parser = SAXParserFactory.newInstance().newSAXParser();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't create SAXParser", e);
      }

    try
      {
      parser.parse( getInputStream(), this);
      }
    catch( SAXParseException spe)
      {
      throw
        new RuntimeException
        ( "Error in document at line=" + spe.getLineNumber(),
          spe);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read GeneratorSet", e);
      }

    return generatorSet_;
    }

  /**
   * Changes the input stream for this reader.
   */
  public void setInputStream( InputStream stream)
    {
    stream_ = stream;
    }

  /**
   * Returns the input stream for this reader.
   */
  protected InputStream getInputStream()
    {
    return stream_;
    }

  /**
   * Pushes the given ElementHandler onto the top of the parse stack.
   */
  protected void pushElementHandler( ElementHandler handler)
    {
    elementHandlers_.add( 0, handler);
    }

  /**
   * Removes an ElementHandler from the top of the parse stack.
   */
  protected ElementHandler popElementHandler()
    {
    return elementHandlers_.size() > 0? (ElementHandler)elementHandlers_.remove( 0) : null;
    }

  /**
   * Returns the ElementHandler at the top of the parse stack.
   */
  protected ElementHandler getCurrentElementHandler()
    {
    return elementHandlers_.size() > 0? (ElementHandler) elementHandlers_.get( 0) : null;
    }
    
  public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
    {
    ElementHandler handler =
      qName.equals( COMBINE_TAG)?         (ElementHandler) new CombineHandler() :
      qName.equals( EXCLUDE_TAG)?         (ElementHandler) new ExcludeHandler() :
      qName.equals( GENERATORS_TAG)?      (ElementHandler) new GeneratorsHandler() :
      qName.equals( INCLUDE_TAG)?         (ElementHandler) new IncludeHandler() :
      qName.equals( TUPLEGENERATOR_TAG)?  (ElementHandler) new TupleGeneratorHandler() :
      null;

    if( handler == null)
      {
      throw new SAXParseException( "Unknown element: " + qName, getDocumentLocator()); 
      }

    ElementHandler parentHandler = getCurrentElementHandler();
    if( !(parentHandler == null
          ? GENERATORS_TAG.equals( qName)
          : parentHandler.isMember( qName)))
      {
      throw new SAXParseException( "The " + qName + " element is not allowed at this location", getDocumentLocator()); 
      }

    handler.setParent( parentHandler);
    pushElementHandler( handler);
    handler.startElement( uri, localName, qName, attributes);
    }

  public void endElement( String uri, String localName, String qName) throws SAXException
    {
    ElementHandler handler = getCurrentElementHandler();
    if( handler != null)
      {
      handler.endElement( uri, localName, qName);
      popElementHandler(); 
      }
    }

  public void setDocumentLocator( Locator locator)
    {
    locator_ = locator;
    }

  public Locator getDocumentLocator()
    {
    return locator_;
    }
    
  public void warning( SAXParseException e) throws SAXException
    {
    }

  public void error( SAXParseException e) throws SAXException
    {
    throw e;
    }

  private InputStream           stream_;
  private Locator               locator_;
  private List<ElementHandler>  elementHandlers_  = new ArrayList<ElementHandler>();
  private GeneratorSet          generatorSet_;

  private static final String COMBINE_TAG         = "Combine";
  private static final String EXCLUDE_TAG         = "Exclude";
  private static final String GENERATORS_TAG      = "Generators";
  private static final String INCLUDE_TAG         = "Include";
  private static final String TUPLEGENERATOR_TAG  = "TupleGenerator";

  private static final String FUNCTION_ATR        = "function";
  private static final String SEED_ATR            = "seed";
  private static final String TUPLES_ATR          = "tuples";
  private static final String VAR_ATR             = "var";
  }
