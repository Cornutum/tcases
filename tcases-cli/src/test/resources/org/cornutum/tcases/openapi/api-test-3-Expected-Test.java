package org.cornutum.examples;

import org.cornutum.testwriter.BaseTest

import org.testng.annotations.Test;

import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class OpenAPIRequestTestCasesTest extends BaseTest {

    @Test
    public void tracePosts_PostIdDefined_Is_Yes() {
        given()
            .cookie( "postId", "A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_2() {
        given()
            .cookie( "postId", "B|C")
        .when()
            .request( "TRACE", "/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_C() {
        given()
            .cookie( "postId", "C")
        .when()
            .request( "TRACE", "/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void tracePosts_PostIdDefined_Is_No() {
        given()
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdType_Is_Null() {
        given()
            .cookie( "postId", "")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdType_Is_NotArray() {
        given()
            .cookie( "postId", "j1")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Type=Not array
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_0() {
        given()
            .cookie( "postId", "")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Size=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_3() {
        given()
            .cookie( "postId", "A|B|C")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Size=3
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsType_Is_Null() {
        given()
            .cookie( "postId", "|A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Contains.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsType_Is_NotString() {
        given()
            .cookie( "postId", "49|qhpdtdbqfk,-320,wrtsmrqtiqkh,843,w,-699.8")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Contains.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_Other() {
        given()
            .cookie( "postId", "[(L&~c@x{Xp7Btyr+R+'p[jrH_o$X`LnQgI'mW?MxX|GcS[=P2R2jAx5z4OLaEDo=8cde$AvZR#F#0>Z[cld8Em_%vE!{(9$+z!(Ghc.{J[m^1HdT/7PKLdR0v:@srfapHf(c/l2s&<{QFg8oMlvF<0@dbS:.")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Contains.Value=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePosts_PostIdItemsUnique_Is_No() {
        given()
            .cookie( "postId", "A|A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Unique=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_Yes() {
        given()
            .pathParam( "attributes", ";approved=true;subject=A Day In Hell;likes=0")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedValue_Is_False() {
        given()
            .pathParam( "attributes", ";approved=false;likes=681280805")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_WhatMeWorry() {
        given()
            .pathParam( "attributes", ";approved=true;subject=What? Me, worry?;likes=0")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_No() {
        given()
            .pathParam( "attributes", "")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_Null() {
        given()
            .pathParam( "attributes", ";attributes")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_NotObject() {
        given()
            .pathParam( "attributes", ";attributes=7^?")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Type=Not object
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        given()
            .pathParam( "attributes", ";likes=944897639")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_Null() {
        given()
            .pathParam( "attributes", ";approved=;likes=515833097")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .pathParam( "attributes", ";approved=cyhdwbn,rUyOPpmo,hvkexe,337.6;likes=1066915565")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Type=Not boolean
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesDefined_Is_No() {
        given()
            .pathParam( "attributes", ";approved=false")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_Null() {
        given()
            .pathParam( "attributes", ";approved=false;likes=")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_NotInteger() {
        given()
            .pathParam( "attributes", ";approved=false;likes=true")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesValue_Is_M1() {
        given()
            .pathParam( "attributes", ";approved=false;likes=-1")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Value.Is=-1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectType_Is_Null() {
        given()
            .pathParam( "attributes", ";approved=false;subject=;likes=462664869")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.subject.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectType_Is_NotString() {
        given()
            .pathParam( "attributes", ";approved=false;subject=cblssshuamos,-395,fswhlbninr,true;likes=867422281")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.subject.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_Other() {
        given()
            .pathParam( "attributes", ";approved=false;subject=7y,|is:=qA}tbaqb^*'U=-\"+DZYOH4|vO,]/c~+:.-g:o)lz~^hL-;. o1\\Vc64a#.Sm!*Y`|qrPD^$*wXi2.a22Qkgt|f8T;#g33QP&F3Sq{<43[l#SP:C>1C~NW?G\\a _%Jkm?u2\"l\"H7*]q:f9@rekaJ{;likes=559447665")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.subject.Value=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesAdditional_Is_Yes() {
        given()
            .pathParam( "attributes", ";approved=false;likes=930884796;kesrtqj=61")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.Additional=Yes
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }
}
