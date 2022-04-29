package org.cornutum.tcases.openapi.restassured;


import org.junit.Test;

import com.github.dreamhead.moco.junit.MocoJunitRunner;
import static com.github.dreamhead.moco.Moco.*;
import org.junit.ClassRule;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class OpenApiTest {

    @ClassRule
    public static MocoJunitRunner runner = MocoJunitRunner.jsonRestRunner( 12306, pathResource( "org/cornutum/tcases/openapi/moco/OpenApiTest-Moco.json"));

    @Test
    public void headPost_UserAttributesDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "user attributes[user-type]", "Typical User")
        .when()
            .request( "HEAD", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeValue_Is_VIP() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "1,2")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsValue_Is_2() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "2,1")
            .queryParam( "user attributes[user-type]", "Typical User")
        .when()
            .request( "HEAD", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void headPost_UserAttributesDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "0,2")
        .when()
            .request( "HEAD", "/post")
        .then()
            // user-attributes.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "user attributes", (String) null)
        .when()
            .request( "HEAD", "/post")
        .then()
            // user-attributes.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesType_Is_NotObject() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "user attributes", "-163.3")
        .when()
            .request( "HEAD", "/post")
        .then()
            // user-attributes.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "0,2")
        .when()
            .request( "HEAD", "/post")
        .then()
            // user-attributes.Value.Properties.user-type.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "user attributes[user-type]", (String) null)
        .when()
            .request( "HEAD", "/post")
        .then()
            // user-attributes.Value.Properties.user-type.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeType_Is_NotString() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "user attributes[user-type]", "-492.0")
        .when()
            .request( "HEAD", "/post")
        .then()
            // user-attributes.Value.Properties.user-type.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeValue_Is_Other() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "user attributes[user-type]", "*76s/1*r")
        .when()
            .request( "HEAD", "/post")
        .then()
            // user-attributes.Value.Properties.user-type.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "user attributes[user-type]", "VIP!")
            .queryParam( "user attributes[wfgawhzebqxw]", "-993.3")
            .queryParam( "user attributes[bt]", "-646")
            .queryParam( "user attributes[d]", "429")
        .when()
            .request( "HEAD", "/post")
        .then()
            // user-attributes.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?", (String) null)
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostType_Is_NotObject() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?", "-689.9")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", (String) null)
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesType_Is_NotArray() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "true")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsSize_Is_1() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "0")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Items.Size=1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsSize_Is_3() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "0,1,2")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", ",1")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", ",,NU,2")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsValue_Is_Other() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "518413185,0")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Items.Contains.Value.Is=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsUnique_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "0,0")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.post-references.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "post?[pfuxkykifiozux]", "g\"YtKR,&")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "/post")
        .then()
            // post.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "Post Marks", "{X}")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_3() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "Post Marks", "<Y> {X} #Z")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Z() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "Post Marks", "#Z")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsUnique_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "Post Marks", "{X} {X} {X}")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "Post Marks", (String) null)
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksType_Is_NotArray() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "Post Marks", "+")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_0() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Size=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_4() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "Post Marks", "<Y> <Y> <Y> {X}")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Size=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsType_Is_NotString() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "Post Marks", "true")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Contains.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Other() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "Post Marks", "#/lL\"%`\\M,7/RPK2]eZ/b\"LtW=MB6,(")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Contains.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "0")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "true")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "884128300094585099.3")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "(?)")
        .when()
            .request( "PUT", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_MeYou() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "0")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "true")
            .formParam( "reviewer", "Me+You")
        .when()
            .request( "PUT", "/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPost_PostIdDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", (String) null)
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdType_Is_NotNumber() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "%")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_M1() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "-1")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "579110988210992054.5")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyMediaType_Is_Other() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "100055597218570470.8")
            .contentType( "application/xml")
            .request().body( ">jg-FQI")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "545768800747318227.7")
            .contentType( "application/x-www-form-urlencoded")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "787781271506673512.7")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "string", "")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "887390451195556957.7")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "218911377319422868.8")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", (String) null)
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "847512139010470218.2")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "eytx", "7ea{")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=Not boolean
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "711003745143914146.9")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "167771822150204639.4")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", (String) null)
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerType_Is_NotString() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "922322478226252041.6")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "loul", "true")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_Other() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "886308504886987482.6")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "i&")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "postId", "920816794015899048.8")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
            .formParam( "dxobuuyffc", "true")
            .formParam( "wssfr", "true")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_1000() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "approved", ".1")
            .pathParam( "userId", ".1000")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "approved", ".0")
            .pathParam( "userId", "")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // userId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "approved", ".0")
            .pathParam( "userId", "")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // userId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdType_Is_NotInteger() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".Ti0q{}")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // userId.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_M1() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".-1")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // userId.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_1001() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".1001")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // userId.Value.Is=1001
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "approved", "")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // approved.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "approved", "")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedType_Is_NotInteger() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "approved", ".true")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // approved.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedValue_Is_Other() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "approved", ".-39383625")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // approved.Value.Is=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "ids", "0")
        .when()
            .request( "GET", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_4() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "ids", "100|93|41|58")
        .when()
            .request( "GET", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPosts_IdsDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "ids", (String) null)
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsType_Is_NotArray() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "ids", "27")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_0() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "ids", "")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Size=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_5() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "ids", "0|85|16|15|7")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Size=5
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "ids", "")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "ids", "%lB,,iZ7s")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_M1() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "ids", "-1")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_101() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "ids", "101")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Value.Is=101
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsUnique_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .queryParam( "ids", "0|82|16|16")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", "0")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_2345() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-Post-Types", "2345,7700")
            .header( "X-User-Id", "305257074")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_7700() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-Post-Types", "7700,1001")
            .header( "X-User-Id", "0")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-User-Id", "633125734")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-Post-Types", "")
            .header( "X-User-Id", "908798277")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesType_Is_NotArray() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-Post-Types", "true")
            .header( "X-User-Id", "45784771")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsSize_Is_1() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-Post-Types", "1001")
            .header( "X-User-Id", "454534268")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Size=1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsSize_Is_3() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-Post-Types", "1001,2345,7700")
            .header( "X-User-Id", "199793883")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-Post-Types", ",1001")
            .header( "X-User-Id", "530075126")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-Post-Types", "true,2345")
            .header( "X-User-Id", "685227360")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_Other() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-Post-Types", "220811161,1001")
            .header( "X-User-Id", "807325043")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Contains.Value.Is=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsUnique_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-Post-Types", "1001,1001")
            .header( "X-User-Id", "212382841")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XUserIdDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-Post-Types", "1001,2345")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XUserIdType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", "")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XUserIdType_Is_NotInteger() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", "EC]h<,G\"3+*")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XUserIdValue_Is_M1() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .header( "X-Post-Types", "1001,7700")
            .header( "X-User-Id", "-1")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_ApprovedDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "true")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"^@z.org\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postPosts_ApprovedValue_Is_False() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "false")
        .when()
            .request( "POST", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_32() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "true")
            .contentType( "application/json")
            .request().body( "{\"text\":\"R)Z=aRk9 O X699I4brEZc)}_H]~YGFQ[u7+[Y{1jvMl\\\\?fP`ZaHE\\\\}tmHpOpb:$\",\"email\":\"7l`.TTd.+7p.TsH.ix;@eGpUw8WI.gov\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postPosts_ApprovedDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
        .when()
            .request( "POST", "/posts")
        .then()
            // approved.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_ApprovedType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", null)
        .when()
            .request( "POST", "/posts")
        .then()
            // approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_ApprovedType_Is_NotBoolean() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "864.8")
        .when()
            .request( "POST", "/posts")
        .then()
            // approved.Type=Not boolean
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyMediaType_Is_Other() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "false")
            .contentType( "application/xml")
            .request().body( "[\"7&)E$\\\\1\",\"b;7\",\"lJ}rpY>H\"]")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "null")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonType_Is_NotObject() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "-394")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.email.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":null}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.email.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailType_Is_NotString() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":{\"ojuslephkogs\":\"O+\\\"q[j\"}}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.email.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_6() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"fR.gov\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.email.Value.Length=6
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_33() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"rROlqhu.sK`/Fa9@P.bYh.ZiC.Ppf.net\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.email.Value.Length=33
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"email\":\"'@e.gov\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.text.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":null,\"email\":\"^@j.org\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.text.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextType_Is_NotString() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":{\"xtgcbcxgfoi\":true,\"rqnrarsvehz\":\"t\",\"kxodoxjnhruqlc\":-55},\"email\":\"f@y.org\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.text.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextValueLength_Is_65() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"5Tr|w`=C8;V~x]FkwIVg-us5r0!]cKW/BU<tb$Q2<fbMlh$?7iFs\\\"Nx$~*VJQWa{@\",\"email\":\"j@9.net\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.text.Value.Length=65
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"z@6.com\",\"i\":true,\"zhbygokxf\":true,\"hbwvniykc\":true}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "f")
            .cookie( "region", "U")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"|@g.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_16() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "3?yf*R>MA(|%pz}@")
            .cookie( "region", "x8U}z3%?N$)|r!FQ")
            .contentType( "text/plain")
            .request().body( "{\"text\":\")@Pxh_%0+3b#tkKys=2JtkX\\\"mXVZ]bCFkngr[zxur1da$%Pz\\\"a>Orl/(Y}a_q-M5\",\"email\":\"8rM.?GYI.3Xq$@8g.sOO.YAD.VG3.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"'@2.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "postId", null)
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"|@4.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdType_Is_NotObject() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "postId", "true")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"O@T.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "region", ")")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"{@0.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", null)
            .cookie( "region", "F")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"=@M.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryType_Is_NotString() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "574.0")
            .cookie( "region", "&")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"b@6.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_0() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "")
            .cookie( "region", "6")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"k@4.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Value.Length=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_17() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "j'AJKsh61`bvz&f^7")
            .cookie( "region", ".")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"r@F.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "<")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"k@b.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "%")
            .cookie( "region", null)
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"r@m.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionType_Is_NotString() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "'")
            .cookie( "region", "611.8")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"J@Q.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionValueLength_Is_0() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "e")
            .cookie( "region", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"I@s.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Value.Length=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionValueLength_Is_17() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "m")
            .cookie( "region", "F~v>f|K%35t[U3nn}")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"P@v.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "W")
            .cookie( "region", "0")
            .cookie( "xtrylgzujvaskrsm", "-+%")
            .cookie( "osgrs", "-625.2")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"=@Q.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "@")
            .cookie( "region", "o")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyMediaType_Is_Other() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "q")
            .cookie( "region", "q")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "string", " (&O;=")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "t")
            .cookie( "region", ".")
            .contentType( "text/plain")
            .request().body( "null")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_NotObject() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", ")")
            .cookie( "region", "H")
            .contentType( "text/plain")
            .request().body( "-463")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "B")
            .cookie( "region", "'")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "U")
            .cookie( "region", "{")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":null}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailType_Is_NotString() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "0")
            .cookie( "region", "e")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":127}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_6() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "V")
            .cookie( "region", "q")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"F7.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Value.Length=6
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_33() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "Y")
            .cookie( "region", "D")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"Ku.#5c._&*.!`~.fPV@j.0c.bm.I4.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Value.Length=33
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "_")
            .cookie( "region", "<")
            .contentType( "text/plain")
            .request().body( "{\"email\":\"F@Y.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "J")
            .cookie( "region", "&")
            .contentType( "text/plain")
            .request().body( "{\"text\":null,\"email\":\"+@J.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextType_Is_NotString() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "%")
            .cookie( "region", "E")
            .contentType( "text/plain")
            .request().body( "{\"text\":13,\"email\":\"w@R.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextValueLength_Is_65() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "#")
            .cookie( "region", "$")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"JPt TLg/#+P9F03USC(F12J)k+BE<&oGTJh<RrmNe6-yTg=G<?qA\\\\MHn+L:c\\\"B7~'\",\"email\":\"q@2.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Value.Length=65
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "country", "O")
            .cookie( "region", "6")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"g@I.org\",\"qlrcnlkwzq\":\"[oIUO\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "postId", "A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_2() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "postId", "B|A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_C() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "postId", "C")
        .when()
            .request( "TRACE", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePosts_PostIdDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "postId", null)
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdType_Is_NotArray() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "postId", "568.0")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_0() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "postId", "")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Size=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_3() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "postId", "A|B|C")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "postId", "|B")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsType_Is_NotString() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "postId", "|B")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Contains.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_Other() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "postId", "jLId'31*^UjExCZerfw#(S_b#m[(L&~c@x{Xp7Btyr+R+'p[jrH_o$X`LnQgI'mW?MxXRGcS[=P2R2jAx5z4OLaEDo=8cde$AvZR#F#0>Z[cld8Em_%vE!{(9$+z!(Ghc.{J[m^1HdT/7PKLdR0v:@srfapHf(c/l2s&<{QFg8oMlvF<0@dbS:.5ef?WOZ+>MHIUD-{?yH$5-DM3*T>hR%ct'(7EzCR]8|A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Contains.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsUnique_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .cookie( "postId", "A|A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", ";approved=true;subject=A Day In Hell;likes=0")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedValue_Is_False() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", ";approved=false;likes=597033979")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_WhatMeWorry() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", ";approved=true;subject=What? Me, worry?;likes=0")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", "")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", "")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_NotObject() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", ";attributes=-441")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", ";likes=1035843385")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", ";approved;likes=127128009")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", ";approved=\"+DZYOH4;likes=222686173")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.approved.Type=Not boolean
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", ";approved=false")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", ";approved=false;likes")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_NotInteger() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", ";approved=false;likes=true")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesValue_Is_M1() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", ";approved=false;likes=-1")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.likes.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", ";approved=false;subject;likes=279878075")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.subject.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectType_Is_NotString() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", ";approved=false;subject=-272;likes=245000076")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.subject.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_Other() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", ";approved=false;subject=~+:.-g:o)lz~^hL-;. o1\\Vc64a#.Sm!*Y`|qrPD^$*wXi2.a22Qkgt|f8T;#g33QP&F3Sq{<43[l#SP:C>1C~NW?G\\a _%Jkm?u2\"l\"H7*]q:f9@re;likes=859073676")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.subject.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "attributes", ";approved=false;likes=776704486;wuf")
        .when()
            .request( "TRACE", "/posts/{attributes}")
        .then()
            // attributes.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "approved=true,likes=0")
            .pathParam( "userId", "0")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "approved=false")
            .pathParam( "userId", "361643225")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "likes=548990533")
            .pathParam( "userId", "0")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // userId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // userId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdType_Is_NotInteger() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "$$jsW^f_,,jxOXXRb")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // userId.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_UserIdValue_Is_M1() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "approved=true")
            .pathParam( "userId", "-1")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // userId.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesDefined_Is_No() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "")
            .pathParam( "userId", "681280805")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "")
            .pathParam( "userId", "357334495")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesType_Is_NotObject() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "-772.4")
            .pathParam( "userId", "471978604")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertyCount_Is_Lt_1() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "")
            .pathParam( "userId", "944897639")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Property-Count=< 1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "approved=")
            .pathParam( "userId", "515833097")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "approved=cyhdwbn,rUyOPpmo,hvkexe,337.6")
            .pathParam( "userId", "1066915565")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.approved.Type=Not boolean
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesType_Is_Null() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "approved=true,likes=")
            .pathParam( "userId", "143212479")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.likes.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesType_Is_NotInteger() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "approved=true,likes=true")
            .pathParam( "userId", "728445295")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.likes.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesLikesValue_Is_M1() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "approved=true,likes=-1")
            .pathParam( "userId", "171293516")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.likes.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deletePostsUserIdAttributes_AttributesValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
            .pathParam( "[attributes]", "approved=true,blsssh=,osdpcfswhlbninro=-981.1,ublzqiqwnlq=true")
            .pathParam( "userId", "814600097")
        .when()
            .request( "DELETE", "/posts/{userId}/{[attributes]}")
        .then()
            // attributes.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getUsers() {
        given()
            .baseUri( forTestServer( "http://localhost:12306"))
        .when()
            .request( "GET", "/users")
        .then()
            .statusCode( isSuccess())
            ;
    }

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private static Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private static String forTestServer( String defaultUri) {
        String testServer = tcasesApiServer();
        return
            defaultUri == null || !testServer.isEmpty()
            ? testServer
            : defaultUri;
    }

    private static String tcasesApiServer() {
        String uri = System.getProperty( "tcasesApiServer");
        return uri == null? "" : uri.trim();
    }
}
