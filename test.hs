{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Database where

import qualified Data.Aeson                 as JSON
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Scientific
import           Data.Text
import           Data.Time
import           Data.UUID
import           GHC.Int
import           Opaleye

---- TYPES FOR TABLE accounts ----

data Account' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 =
  Account
    { accountId :: c1
    , accountName :: c2
    , accountDescription :: c3
    , accountDomain :: c4
    , accountWebsite :: c5
    , accountLogo :: c6
    , accountAddress1 :: c7
    , accountAddress2 :: c8
    , accountCity :: c9
    , accountRegion :: c10
    , accountPostalCode :: c11
    , accountCountryCode :: c12
    , accountPhone :: c13
    , accountLegalName :: c14
    , accountFounded :: c15
    , accountEmployees :: c16
    , accountRaised :: c17
    , accountCreatedAt :: c18
    , accountUpdatedAt :: c19
    , accountTimeZone :: c20
    , accountUuid :: c21
    , accountApiKey :: c22
    , accountSetupCompletedAt :: c23
    , accountStripeId :: c24
    , accountStripeSubscriptionId :: c25
    , accountCreditCardLastFour :: c26
    , accountCreditCardExpirationMonth :: c27
    , accountCreditCardExpirationYear :: c28
    , accountCreditCardType :: c29
    , accountPlan :: c30
    , accountTrialStartAt :: c31
    , accountTrialEndAt :: c32
    , accountSubscriptionStartAt :: c33
    , accountSubscriptionEndAt :: c34
    , accountCoupon :: c35
    , accountChurnSlackChannel :: c36
    , accountSignupSlackChannel :: c37
    , accountBrightjsId :: c38
    }

type Account = Account' Int32 (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) LocalTime LocalTime (Maybe Text) (Maybe UUID) (Maybe Text) (Maybe LocalTime) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe LocalTime) (Maybe LocalTime) (Maybe LocalTime) (Maybe LocalTime) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text)

type AccountReadColumns = Account' (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column PGTimestamp) (Column PGTimestamp) (Column (Nullable PGText)) (Column (Nullable PGUuid)) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type AccountWriteColumns = Account' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Column PGTimestamp) (Column PGTimestamp) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGUuid))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText)))

$(makeAdaptorAndInstance "pAccount" ''Account')

accountTable :: Table AccountWriteColumns AccountReadColumns
accountTable = Table "accounts" (pAccount
  Account
    { accountId = optional "id"
    , accountName = optional "name"
    , accountDescription = optional "description"
    , accountDomain = optional "domain"
    , accountWebsite = optional "website"
    , accountLogo = optional "logo"
    , accountAddress1 = optional "address1"
    , accountAddress2 = optional "address2"
    , accountCity = optional "city"
    , accountRegion = optional "region"
    , accountPostalCode = optional "postal_code"
    , accountCountryCode = optional "country_code"
    , accountPhone = optional "phone"
    , accountLegalName = optional "legal_name"
    , accountFounded = optional "founded"
    , accountEmployees = optional "employees"
    , accountRaised = optional "raised"
    , accountCreatedAt = required "created_at"
    , accountUpdatedAt = required "updated_at"
    , accountTimeZone = optional "time_zone"
    , accountUuid = optional "uuid"
    , accountApiKey = optional "api_key"
    , accountSetupCompletedAt = optional "setup_completed_at"
    , accountStripeId = optional "stripe_id"
    , accountStripeSubscriptionId = optional "stripe_subscription_id"
    , accountCreditCardLastFour = optional "credit_card_last_four"
    , accountCreditCardExpirationMonth = optional "credit_card_expiration_month"
    , accountCreditCardExpirationYear = optional "credit_card_expiration_year"
    , accountCreditCardType = optional "credit_card_type"
    , accountPlan = optional "plan"
    , accountTrialStartAt = optional "trial_start_at"
    , accountTrialEndAt = optional "trial_end_at"
    , accountSubscriptionStartAt = optional "subscription_start_at"
    , accountSubscriptionEndAt = optional "subscription_end_at"
    , accountCoupon = optional "coupon"
    , accountChurnSlackChannel = optional "churn_slack_channel"
    , accountSignupSlackChannel = optional "signup_slack_channel"
    , accountBrightjsId = optional "brightjs_id"
    }
  )

---- TYPES FOR TABLE authorizations ----

data Authorization' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 =
  Authorization
    { authorizationId :: c1
    , authorizationApiKey :: c2
    , authorizationBaseUri :: c3
    , authorizationService :: c4
    , authorizationAccountId :: c5
    , authorizationCreatedAt :: c6
    , authorizationUpdatedAt :: c7
    , authorizationSetupByUserId :: c8
    , authorizationToken :: c9
    , authorizationSecret :: c10
    , authorizationUid :: c11
    , authorizationBillingProvider :: c12
    }

type Authorization = Authorization' Int32 (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe LocalTime) (Maybe LocalTime) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Bool)

type AuthorizationReadColumns = Authorization' (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGBool))

type AuthorizationWriteColumns = Authorization' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGBool)))

$(makeAdaptorAndInstance "pAuthorization" ''Authorization')

authorizationTable :: Table AuthorizationWriteColumns AuthorizationReadColumns
authorizationTable = Table "authorizations" (pAuthorization
  Authorization
    { authorizationId = optional "id"
    , authorizationApiKey = optional "api_key"
    , authorizationBaseUri = optional "base_uri"
    , authorizationService = optional "service"
    , authorizationAccountId = optional "account_id"
    , authorizationCreatedAt = optional "created_at"
    , authorizationUpdatedAt = optional "updated_at"
    , authorizationSetupByUserId = optional "setup_by_user_id"
    , authorizationToken = optional "token"
    , authorizationSecret = optional "secret"
    , authorizationUid = optional "uid"
    , authorizationBillingProvider = optional "billing_provider"
    }
  )

---- TYPES FOR TABLE brightjs_referrers ----

data BrightjsReferrer' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 =
  BrightjsReferrer
    { brightjsReferrerId :: c1
    , brightjsReferrerBrightjsUserIdId :: c2
    , brightjsReferrerReferrerUrl :: c3
    , brightjsReferrerReferrerHost :: c4
    , brightjsReferrerReferredAt :: c5
    , brightjsReferrerCreatedAt :: c6
    , brightjsReferrerUpdatedAt :: c7
    , brightjsReferrerReferrerType :: c8
    , brightjsReferrerReferrerFrom :: c9
    , brightjsReferrerCampaignName :: c10
    , brightjsReferrerCampaignSource :: c11
    , brightjsReferrerCampaignTerm :: c12
    , brightjsReferrerCampaignMedium :: c13
    , brightjsReferrerCampaignContent :: c14
    , brightjsReferrerUrl :: c15
    }

type BrightjsReferrer = BrightjsReferrer' Int32 (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe LocalTime) LocalTime LocalTime (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text)

type BrightjsReferrerReadColumns = BrightjsReferrer' (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column PGTimestamp) (Column PGTimestamp) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type BrightjsReferrerWriteColumns = BrightjsReferrer' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamp))) (Column PGTimestamp) (Column PGTimestamp) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText)))

$(makeAdaptorAndInstance "pBrightjsReferrer" ''BrightjsReferrer')

brightjsReferrerTable :: Table BrightjsReferrerWriteColumns BrightjsReferrerReadColumns
brightjsReferrerTable = Table "brightjs_referrers" (pBrightjsReferrer
  BrightjsReferrer
    { brightjsReferrerId = optional "id"
    , brightjsReferrerBrightjsUserIdId = optional "brightjs_user_id_id"
    , brightjsReferrerReferrerUrl = optional "referrer_url"
    , brightjsReferrerReferrerHost = optional "referrer_host"
    , brightjsReferrerReferredAt = optional "referred_at"
    , brightjsReferrerCreatedAt = required "created_at"
    , brightjsReferrerUpdatedAt = required "updated_at"
    , brightjsReferrerReferrerType = optional "referrer_type"
    , brightjsReferrerReferrerFrom = optional "referrer_from"
    , brightjsReferrerCampaignName = optional "campaign_name"
    , brightjsReferrerCampaignSource = optional "campaign_source"
    , brightjsReferrerCampaignTerm = optional "campaign_term"
    , brightjsReferrerCampaignMedium = optional "campaign_medium"
    , brightjsReferrerCampaignContent = optional "campaign_content"
    , brightjsReferrerUrl = optional "url"
    }
  )

---- TYPES FOR TABLE brightjs_user_ids ----

data BrightjsUserId' c1 c2 c3 c4 c5 =
  BrightjsUserId
    { brightjsUserIdId :: c1
    , brightjsUserIdOrganizationId :: c2
    , brightjsUserIdBrightjsUserId :: c3
    , brightjsUserIdCreatedAt :: c4
    , brightjsUserIdUpdatedAt :: c5
    }

type BrightjsUserId = BrightjsUserId' Int32 (Maybe Int32) (Maybe Text) LocalTime LocalTime

type BrightjsUserIdReadColumns = BrightjsUserId' (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column PGTimestamp) (Column PGTimestamp)

type BrightjsUserIdWriteColumns = BrightjsUserId' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Column PGTimestamp) (Column PGTimestamp)

$(makeAdaptorAndInstance "pBrightjsUserId" ''BrightjsUserId')

brightjsUserIdTable :: Table BrightjsUserIdWriteColumns BrightjsUserIdReadColumns
brightjsUserIdTable = Table "brightjs_user_ids" (pBrightjsUserId
  BrightjsUserId
    { brightjsUserIdId = optional "id"
    , brightjsUserIdOrganizationId = optional "organization_id"
    , brightjsUserIdBrightjsUserId = optional "brightjs_user_id"
    , brightjsUserIdCreatedAt = required "created_at"
    , brightjsUserIdUpdatedAt = required "updated_at"
    }
  )

---- TYPES FOR TABLE chargify_events ----

data ChargifyEvent' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 =
  ChargifyEvent
    { chargifyEventId :: c1
    , chargifyEventBillingSystemId :: c2
    , chargifyEventOccurredAt :: c3
    , chargifyEventKey :: c4
    , chargifyEventMessage :: c5
    , chargifyEventMetadata :: c6
    , chargifyEventSubscriptionId :: c7
    , chargifyEventCreatedAt :: c8
    , chargifyEventUpdatedAt :: c9
    , chargifyEventProcessedAt :: c10
    , chargifyEventAccountId :: c11
    , chargifyEventDeferredAt :: c12
    , chargifyEventErrantAt :: c13
    }

type ChargifyEvent = ChargifyEvent' Int32 (Maybe Text) (Maybe LocalTime) (Maybe Text) (Maybe Text) (Maybe JSON.Value) (Maybe Int32) LocalTime LocalTime (Maybe LocalTime) (Maybe Int32) (Maybe LocalTime) (Maybe LocalTime)

type ChargifyEventReadColumns = ChargifyEvent' (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGJson)) (Column (Nullable PGInt4)) (Column PGTimestamp) (Column PGTimestamp) (Column (Nullable PGTimestamp)) (Column (Nullable PGInt4)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp))

type ChargifyEventWriteColumns = ChargifyEvent' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGJson))) (Maybe (Column (Nullable PGInt4))) (Column PGTimestamp) (Column PGTimestamp) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp)))

$(makeAdaptorAndInstance "pChargifyEvent" ''ChargifyEvent')

chargifyEventTable :: Table ChargifyEventWriteColumns ChargifyEventReadColumns
chargifyEventTable = Table "chargify_events" (pChargifyEvent
  ChargifyEvent
    { chargifyEventId = optional "id"
    , chargifyEventBillingSystemId = optional "billing_system_id"
    , chargifyEventOccurredAt = optional "occurred_at"
    , chargifyEventKey = optional "key"
    , chargifyEventMessage = optional "message"
    , chargifyEventMetadata = optional "metadata"
    , chargifyEventSubscriptionId = optional "subscription_id"
    , chargifyEventCreatedAt = required "created_at"
    , chargifyEventUpdatedAt = required "updated_at"
    , chargifyEventProcessedAt = optional "processed_at"
    , chargifyEventAccountId = optional "account_id"
    , chargifyEventDeferredAt = optional "deferred_at"
    , chargifyEventErrantAt = optional "errant_at"
    }
  )

---- TYPES FOR TABLE components ----

data Component' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 =
  Component
    { componentId :: c1
    , componentBillingSystemId :: c2
    , componentBillingSystemGroupId :: c3
    , componentName :: c4
    , componentPricingMethod :: c5
    , componentUnitName :: c6
    , componentUnitPrice :: c7
    , componentKind :: c8
    , componentPrices :: c9
    , componentArchived :: c10
    , componentCreatedAt :: c11
    , componentUpdatedAt :: c12
    , componentService :: c13
    , componentAccountId :: c14
    }

type Component = Component' Int32 (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Scientific) (Maybe Text) (Maybe JSON.Value) (Maybe Bool) LocalTime LocalTime (Maybe Int32) (Maybe Int32)

type ComponentReadColumns = Component' (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGNumeric)) (Column (Nullable PGText)) (Column (Nullable PGJson)) (Column (Nullable PGBool)) (Column PGTimestamp) (Column PGTimestamp) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type ComponentWriteColumns = Component' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGJson))) (Maybe (Column (Nullable PGBool))) (Column PGTimestamp) (Column PGTimestamp) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4)))

$(makeAdaptorAndInstance "pComponent" ''Component')

componentTable :: Table ComponentWriteColumns ComponentReadColumns
componentTable = Table "components" (pComponent
  Component
    { componentId = optional "id"
    , componentBillingSystemId = optional "billing_system_id"
    , componentBillingSystemGroupId = optional "billing_system_group_id"
    , componentName = optional "name"
    , componentPricingMethod = optional "pricing_method"
    , componentUnitName = optional "unit_name"
    , componentUnitPrice = optional "unit_price"
    , componentKind = optional "kind"
    , componentPrices = optional "prices"
    , componentArchived = optional "archived"
    , componentCreatedAt = required "created_at"
    , componentUpdatedAt = required "updated_at"
    , componentService = optional "service"
    , componentAccountId = optional "account_id"
    }
  )

---- TYPES FOR TABLE coupons ----

data Coupon' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 =
  Coupon
    { couponId :: c1
    , couponBillingSystemId :: c2
    , couponName :: c3
    , couponDescription :: c4
    , couponCode :: c5
    , couponDuration :: c6
    , couponDurationInMonths :: c7
    , couponAmountOff :: c8
    , couponPercentOff :: c9
    , couponStartAt :: c10
    , couponEndAt :: c11
    , couponRedemptionLimit :: c12
    , couponCreatedAt :: c13
    , couponUpdatedAt :: c14
    , couponNumberOfRedemptions :: c15
    , couponRevenueGenerated :: c16
    , couponCurrentMrr :: c17
    , couponCurrentCustomers :: c18
    , couponService :: c19
    , couponSubcodes :: c20
    , couponAccountId :: c21
    , couponDeletedAt :: c22
    , couponCurrency :: c23
    }

type Coupon = Coupon' Int32 (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Scientific) (Maybe Int32) (Maybe LocalTime) (Maybe LocalTime) (Maybe Int32) LocalTime LocalTime (Maybe Int32) (Maybe Scientific) (Maybe Scientific) (Maybe Int32) (Maybe Int32) ([Text]) (Maybe Int32) (Maybe LocalTime) (Maybe Text)

type CouponReadColumns = Coupon' (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGNumeric)) (Column (Nullable PGInt4)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGInt4)) (Column PGTimestamp) (Column PGTimestamp) (Column (Nullable PGInt4)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable (PGArray Text))) (Column (Nullable PGInt4)) (Column (Nullable PGTimestamp)) (Column (Nullable PGText))

type CouponWriteColumns = Coupon' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGInt4))) (Column PGTimestamp) (Column PGTimestamp) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable (PGArray Text)))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGText)))

$(makeAdaptorAndInstance "pCoupon" ''Coupon')

couponTable :: Table CouponWriteColumns CouponReadColumns
couponTable = Table "coupons" (pCoupon
  Coupon
    { couponId = optional "id"
    , couponBillingSystemId = optional "billing_system_id"
    , couponName = optional "name"
    , couponDescription = optional "description"
    , couponCode = optional "code"
    , couponDuration = optional "duration"
    , couponDurationInMonths = optional "duration_in_months"
    , couponAmountOff = optional "amount_off"
    , couponPercentOff = optional "percent_off"
    , couponStartAt = optional "start_at"
    , couponEndAt = optional "end_at"
    , couponRedemptionLimit = optional "redemption_limit"
    , couponCreatedAt = required "created_at"
    , couponUpdatedAt = required "updated_at"
    , couponNumberOfRedemptions = optional "number_of_redemptions"
    , couponRevenueGenerated = optional "revenue_generated"
    , couponCurrentMrr = optional "current_mrr"
    , couponCurrentCustomers = optional "current_customers"
    , couponService = optional "service"
    , couponSubcodes = optional "subcodes"
    , couponAccountId = optional "account_id"
    , couponDeletedAt = optional "deleted_at"
    , couponCurrency = optional "currency"
    }
  )

---- TYPES FOR TABLE events ----

data Event' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 =
  Event
    { eventId :: c1
    , eventType :: c2
    , eventOccurredAt :: c3
    , eventAmount :: c4
    , eventMetadata :: c5
    , eventService :: c6
    , eventAccountId :: c7
    , eventOrganizationId :: c8
    , eventSubscriptionId :: c9
    , eventPlanId :: c10
    , eventComponentId :: c11
    , eventCouponId :: c12
    , eventCreatedAt :: c13
    , eventUpdatedAt :: c14
    , eventValidatedAt :: c15
    }

type Event = Event' Int32 (Maybe Text) (Maybe LocalTime) (Maybe Scientific) (Maybe JSON.Value) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) LocalTime LocalTime (Maybe LocalTime)

type EventReadColumns = Event' (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGNumeric)) (Column (Nullable PGJson)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column PGTimestamp) (Column PGTimestamp) (Column (Nullable PGTimestamp))

type EventWriteColumns = Event' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGJson))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Column PGTimestamp) (Column PGTimestamp) (Maybe (Column (Nullable PGTimestamp)))

$(makeAdaptorAndInstance "pEvent" ''Event')

eventTable :: Table EventWriteColumns EventReadColumns
eventTable = Table "events" (pEvent
  Event
    { eventId = optional "id"
    , eventType = optional "type"
    , eventOccurredAt = optional "occurred_at"
    , eventAmount = optional "amount"
    , eventMetadata = optional "metadata"
    , eventService = optional "service"
    , eventAccountId = optional "account_id"
    , eventOrganizationId = optional "organization_id"
    , eventSubscriptionId = optional "subscription_id"
    , eventPlanId = optional "plan_id"
    , eventComponentId = optional "component_id"
    , eventCouponId = optional "coupon_id"
    , eventCreatedAt = required "created_at"
    , eventUpdatedAt = required "updated_at"
    , eventValidatedAt = optional "validated_at"
    }
  )

---- TYPES FOR TABLE imports ----

data Import' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 =
  Import
    { importId :: c1
    , importAccountId :: c2
    , importUserId :: c3
    , importService :: c4
    , importJobIdentifier :: c5
    , importStartedAt :: c6
    , importFinishedAt :: c7
    , importRecordsToImport :: c8
    , importRecordsImported :: c9
    , importCreatedAt :: c10
    , importUpdatedAt :: c11
    , importTimePerRecord :: c12
    }

type Import = Import' Int32 (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe LocalTime) (Maybe LocalTime) (Maybe Int32) (Maybe Int32) LocalTime LocalTime (Maybe Double)

type ImportReadColumns = Import' (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column PGTimestamp) (Column PGTimestamp) (Column (Nullable PGFloat8))

type ImportWriteColumns = Import' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Column PGTimestamp) (Column PGTimestamp) (Maybe (Column (Nullable PGFloat8)))

$(makeAdaptorAndInstance "pImport" ''Import')

importTable :: Table ImportWriteColumns ImportReadColumns
importTable = Table "imports" (pImport
  Import
    { importId = optional "id"
    , importAccountId = optional "account_id"
    , importUserId = optional "user_id"
    , importService = optional "service"
    , importJobIdentifier = optional "job_identifier"
    , importStartedAt = optional "started_at"
    , importFinishedAt = optional "finished_at"
    , importRecordsToImport = optional "records_to_import"
    , importRecordsImported = optional "records_imported"
    , importCreatedAt = required "created_at"
    , importUpdatedAt = required "updated_at"
    , importTimePerRecord = optional "time_per_record"
    }
  )

---- TYPES FOR TABLE invoice_line_items ----

data InvoiceLineItem' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 =
  InvoiceLineItem
    { invoiceLineItemId :: c1
    , invoiceLineItemService :: c2
    , invoiceLineItemBillingSystemId :: c3
    , invoiceLineItemAccountId :: c4
    , invoiceLineItemOrganizationId :: c5
    , invoiceLineItemSubscriptionId :: c6
    , invoiceLineItemInvoiceId :: c7
    , invoiceLineItemPlanId :: c8
    , invoiceLineItemAmount :: c9
    , invoiceLineItemCurrency :: c10
    , invoiceLineItemDescription :: c11
    , invoiceLineItemDiscountable :: c12
    , invoiceLineItemMetadata :: c13
    , invoiceLineItemPeriodStartAt :: c14
    , invoiceLineItemPeriodEndAt :: c15
    , invoiceLineItemProration :: c16
    , invoiceLineItemQuantity :: c17
    , invoiceLineItemType :: c18
    , invoiceLineItemCreatedAt :: c19
    , invoiceLineItemUpdatedAt :: c20
    }

type InvoiceLineItem = InvoiceLineItem' Int32 (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Scientific) (Maybe Text) (Maybe Text) (Maybe Bool) (Maybe JSON.Value) (Maybe LocalTime) (Maybe LocalTime) (Maybe Bool) (Maybe Int32) (Maybe Text) LocalTime LocalTime

type InvoiceLineItemReadColumns = InvoiceLineItem' (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGNumeric)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGBool)) (Column (Nullable PGJson)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGBool)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column PGTimestamp) (Column PGTimestamp)

type InvoiceLineItemWriteColumns = InvoiceLineItem' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGBool))) (Maybe (Column (Nullable PGJson))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGBool))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Column PGTimestamp) (Column PGTimestamp)

$(makeAdaptorAndInstance "pInvoiceLineItem" ''InvoiceLineItem')

invoiceLineItemTable :: Table InvoiceLineItemWriteColumns InvoiceLineItemReadColumns
invoiceLineItemTable = Table "invoice_line_items" (pInvoiceLineItem
  InvoiceLineItem
    { invoiceLineItemId = optional "id"
    , invoiceLineItemService = optional "service"
    , invoiceLineItemBillingSystemId = optional "billing_system_id"
    , invoiceLineItemAccountId = optional "account_id"
    , invoiceLineItemOrganizationId = optional "organization_id"
    , invoiceLineItemSubscriptionId = optional "subscription_id"
    , invoiceLineItemInvoiceId = optional "invoice_id"
    , invoiceLineItemPlanId = optional "plan_id"
    , invoiceLineItemAmount = optional "amount"
    , invoiceLineItemCurrency = optional "currency"
    , invoiceLineItemDescription = optional "description"
    , invoiceLineItemDiscountable = optional "discountable"
    , invoiceLineItemMetadata = optional "metadata"
    , invoiceLineItemPeriodStartAt = optional "period_start_at"
    , invoiceLineItemPeriodEndAt = optional "period_end_at"
    , invoiceLineItemProration = optional "proration"
    , invoiceLineItemQuantity = optional "quantity"
    , invoiceLineItemType = optional "type"
    , invoiceLineItemCreatedAt = required "created_at"
    , invoiceLineItemUpdatedAt = required "updated_at"
    }
  )

---- TYPES FOR TABLE invoices ----

data Invoice' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 =
  Invoice
    { invoiceId :: c1
    , invoiceBillingSystemId :: c2
    , invoiceAccountId :: c3
    , invoiceOrganizationId :: c4
    , invoiceSubscriptionId :: c5
    , invoiceOpenedAt :: c6
    , invoiceClosedAt :: c7
    , invoiceSettledAt :: c8
    , invoiceStartingBalance :: c9
    , invoiceEndingBalance :: c10
    , invoiceTotal :: c11
    , invoiceBillingSystemCreatedAt :: c12
    , invoiceCreatedAt :: c13
    , invoiceUpdatedAt :: c14
    , invoiceService :: c15
    , invoiceCurrency :: c16
    , invoiceAmountDue :: c17
    , invoiceCouponId :: c18
    , invoiceDiscountPercentOff :: c19
    , invoiceDiscountAmountOff :: c20
    , invoiceSubtotal :: c21
    }

type Invoice = Invoice' Int32 (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe LocalTime) (Maybe LocalTime) (Maybe LocalTime) (Maybe Scientific) (Maybe Scientific) (Maybe Scientific) (Maybe LocalTime) LocalTime LocalTime (Maybe Int32) (Maybe Text) (Maybe Scientific) (Maybe Int32) (Maybe Int32) (Maybe Scientific) (Maybe Scientific)

type InvoiceReadColumns = Invoice' (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column (Nullable PGTimestamp)) (Column PGTimestamp) (Column PGTimestamp) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGNumeric)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric))

type InvoiceWriteColumns = Invoice' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGTimestamp))) (Column PGTimestamp) (Column PGTimestamp) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric)))

$(makeAdaptorAndInstance "pInvoice" ''Invoice')

invoiceTable :: Table InvoiceWriteColumns InvoiceReadColumns
invoiceTable = Table "invoices" (pInvoice
  Invoice
    { invoiceId = optional "id"
    , invoiceBillingSystemId = optional "billing_system_id"
    , invoiceAccountId = optional "account_id"
    , invoiceOrganizationId = optional "organization_id"
    , invoiceSubscriptionId = optional "subscription_id"
    , invoiceOpenedAt = optional "opened_at"
    , invoiceClosedAt = optional "closed_at"
    , invoiceSettledAt = optional "settled_at"
    , invoiceStartingBalance = optional "starting_balance"
    , invoiceEndingBalance = optional "ending_balance"
    , invoiceTotal = optional "total"
    , invoiceBillingSystemCreatedAt = optional "billing_system_created_at"
    , invoiceCreatedAt = required "created_at"
    , invoiceUpdatedAt = required "updated_at"
    , invoiceService = optional "service"
    , invoiceCurrency = optional "currency"
    , invoiceAmountDue = optional "amount_due"
    , invoiceCouponId = optional "coupon_id"
    , invoiceDiscountPercentOff = optional "discount_percent_off"
    , invoiceDiscountAmountOff = optional "discount_amount_off"
    , invoiceSubtotal = optional "subtotal"
    }
  )

---- TYPES FOR TABLE metrics ----

data Metric' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 =
  Metric
    { metricId :: c1
    , metricAccountId :: c2
    , metricDate :: c3
    , metricNewMrr :: c4
    , metricDiscountRollOffMrr :: c5
    , metricExpansionMrr :: c6
    , metricReactivationMrr :: c7
    , metricContractionMrr :: c8
    , metricChurnMrr :: c9
    , metricNetMrrChange :: c10
    , metricTotalMrr :: c11
    , metricNewCustomers :: c12
    , metricLostCustomers :: c13
    , metricCreatedAt :: c14
    , metricUpdatedAt :: c15
    , metricTotalCustomers :: c16
    , metricReactivatedCustomers :: c17
    }

type Metric = Metric' Int32 (Maybe Int32) (Maybe Day) (Maybe Scientific) (Maybe Scientific) (Maybe Scientific) (Maybe Scientific) (Maybe Scientific) (Maybe Scientific) (Maybe Scientific) (Maybe Scientific) (Maybe Int32) (Maybe Int32) LocalTime LocalTime (Maybe Int32) (Maybe Int32)

type MetricReadColumns = Metric' (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGDate)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column PGTimestamp) (Column PGTimestamp) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type MetricWriteColumns = Metric' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGDate))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Column PGTimestamp) (Column PGTimestamp) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4)))

$(makeAdaptorAndInstance "pMetric" ''Metric')

metricTable :: Table MetricWriteColumns MetricReadColumns
metricTable = Table "metrics" (pMetric
  Metric
    { metricId = optional "id"
    , metricAccountId = optional "account_id"
    , metricDate = optional "date"
    , metricNewMrr = optional "new_mrr"
    , metricDiscountRollOffMrr = optional "discount_roll_off_mrr"
    , metricExpansionMrr = optional "expansion_mrr"
    , metricReactivationMrr = optional "reactivation_mrr"
    , metricContractionMrr = optional "contraction_mrr"
    , metricChurnMrr = optional "churn_mrr"
    , metricNetMrrChange = optional "net_mrr_change"
    , metricTotalMrr = optional "total_mrr"
    , metricNewCustomers = optional "new_customers"
    , metricLostCustomers = optional "lost_customers"
    , metricCreatedAt = required "created_at"
    , metricUpdatedAt = required "updated_at"
    , metricTotalCustomers = optional "total_customers"
    , metricReactivatedCustomers = optional "reactivated_customers"
    }
  )

---- TYPES FOR TABLE organizations ----

data Organization' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 =
  Organization
    { organizationId :: c1
    , organizationName :: c2
    , organizationFirstName :: c3
    , organizationLastName :: c4
    , organizationEmail :: c5
    , organizationCustomerId :: c6
    , organizationBillingSystemId :: c7
    , organizationBillingSystemCreatedAt :: c8
    , organizationBillingSystemUpdatedAt :: c9
    , organizationAddress1 :: c10
    , organizationAddress2 :: c11
    , organizationCity :: c12
    , organizationRegion :: c13
    , organizationPostalCode :: c14
    , organizationCountryCode :: c15
    , organizationVatNumber :: c16
    , organizationPhone :: c17
    , organizationWebsite :: c18
    , organizationCreatedAt :: c19
    , organizationUpdatedAt :: c20
    , organizationAccountId :: c21
    , organizationService :: c22
    , organizationTotalRevenue :: c23
    , organizationLastSubscriptionActivatedAt :: c24
    }

type Organization = Organization' Int32 (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe LocalTime) (Maybe LocalTime) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) LocalTime LocalTime (Maybe Int32) (Maybe Int32) (Maybe Scientific) (Maybe LocalTime)

type OrganizationReadColumns = Organization' (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column PGTimestamp) (Column PGTimestamp) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGNumeric)) (Column (Nullable PGTimestamp))

type OrganizationWriteColumns = Organization' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Column PGTimestamp) (Column PGTimestamp) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGTimestamp)))

$(makeAdaptorAndInstance "pOrganization" ''Organization')

organizationTable :: Table OrganizationWriteColumns OrganizationReadColumns
organizationTable = Table "organizations" (pOrganization
  Organization
    { organizationId = optional "id"
    , organizationName = optional "name"
    , organizationFirstName = optional "first_name"
    , organizationLastName = optional "last_name"
    , organizationEmail = optional "email"
    , organizationCustomerId = optional "customer_id"
    , organizationBillingSystemId = optional "billing_system_id"
    , organizationBillingSystemCreatedAt = optional "billing_system_created_at"
    , organizationBillingSystemUpdatedAt = optional "billing_system_updated_at"
    , organizationAddress1 = optional "address1"
    , organizationAddress2 = optional "address2"
    , organizationCity = optional "city"
    , organizationRegion = optional "region"
    , organizationPostalCode = optional "postal_code"
    , organizationCountryCode = optional "country_code"
    , organizationVatNumber = optional "vat_number"
    , organizationPhone = optional "phone"
    , organizationWebsite = optional "website"
    , organizationCreatedAt = required "created_at"
    , organizationUpdatedAt = required "updated_at"
    , organizationAccountId = optional "account_id"
    , organizationService = optional "service"
    , organizationTotalRevenue = optional "total_revenue"
    , organizationLastSubscriptionActivatedAt = optional "last_subscription_activated_at"
    }
  )

---- TYPES FOR TABLE pending_invoice_items ----

data PendingInvoiceItem' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 =
  PendingInvoiceItem
    { pendingInvoiceItemId :: c1
    , pendingInvoiceItemService :: c2
    , pendingInvoiceItemBillingSystemId :: c3
    , pendingInvoiceItemAccountId :: c4
    , pendingInvoiceItemOrganizationId :: c5
    , pendingInvoiceItemSubscriptionId :: c6
    , pendingInvoiceItemPlanId :: c7
    , pendingInvoiceItemInvoiceId :: c8
    , pendingInvoiceItemAmount :: c9
    , pendingInvoiceItemCurrency :: c10
    , pendingInvoiceItemRecordedAt :: c11
    , pendingInvoiceItemPeriodStartAt :: c12
    , pendingInvoiceItemPeriodEndAt :: c13
    , pendingInvoiceItemDescription :: c14
    , pendingInvoiceItemDiscountable :: c15
    , pendingInvoiceItemProration :: c16
    , pendingInvoiceItemQuantity :: c17
    , pendingInvoiceItemMetadata :: c18
    , pendingInvoiceItemCreatedAt :: c19
    , pendingInvoiceItemUpdatedAt :: c20
    }

type PendingInvoiceItem = PendingInvoiceItem' Int32 (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Scientific) (Maybe Text) (Maybe LocalTime) (Maybe LocalTime) (Maybe LocalTime) (Maybe Text) (Maybe Bool) (Maybe Bool) (Maybe Int32) (Maybe JSON.Value) LocalTime LocalTime

type PendingInvoiceItemReadColumns = PendingInvoiceItem' (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGNumeric)) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGText)) (Column (Nullable PGBool)) (Column (Nullable PGBool)) (Column (Nullable PGInt4)) (Column (Nullable PGJson)) (Column PGTimestamp) (Column PGTimestamp)

type PendingInvoiceItemWriteColumns = PendingInvoiceItem' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGBool))) (Maybe (Column (Nullable PGBool))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGJson))) (Column PGTimestamp) (Column PGTimestamp)

$(makeAdaptorAndInstance "pPendingInvoiceItem" ''PendingInvoiceItem')

pendingInvoiceItemTable :: Table PendingInvoiceItemWriteColumns PendingInvoiceItemReadColumns
pendingInvoiceItemTable = Table "pending_invoice_items" (pPendingInvoiceItem
  PendingInvoiceItem
    { pendingInvoiceItemId = optional "id"
    , pendingInvoiceItemService = optional "service"
    , pendingInvoiceItemBillingSystemId = optional "billing_system_id"
    , pendingInvoiceItemAccountId = optional "account_id"
    , pendingInvoiceItemOrganizationId = optional "organization_id"
    , pendingInvoiceItemSubscriptionId = optional "subscription_id"
    , pendingInvoiceItemPlanId = optional "plan_id"
    , pendingInvoiceItemInvoiceId = optional "invoice_id"
    , pendingInvoiceItemAmount = optional "amount"
    , pendingInvoiceItemCurrency = optional "currency"
    , pendingInvoiceItemRecordedAt = optional "recorded_at"
    , pendingInvoiceItemPeriodStartAt = optional "period_start_at"
    , pendingInvoiceItemPeriodEndAt = optional "period_end_at"
    , pendingInvoiceItemDescription = optional "description"
    , pendingInvoiceItemDiscountable = optional "discountable"
    , pendingInvoiceItemProration = optional "proration"
    , pendingInvoiceItemQuantity = optional "quantity"
    , pendingInvoiceItemMetadata = optional "metadata"
    , pendingInvoiceItemCreatedAt = required "created_at"
    , pendingInvoiceItemUpdatedAt = required "updated_at"
    }
  )

---- TYPES FOR TABLE pg_stat_statements ----

data PgStatStatement' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 =
  PgStatStatement
    { pgStatStatementUserid :: c1
    , pgStatStatementDbid :: c2
    , pgStatStatementQueryid :: c3
    , pgStatStatementQuery :: c4
    , pgStatStatementCalls :: c5
    , pgStatStatementTotalTime :: c6
    , pgStatStatementMinTime :: c7
    , pgStatStatementMaxTime :: c8
    , pgStatStatementMeanTime :: c9
    , pgStatStatementStddevTime :: c10
    , pgStatStatementRows :: c11
    , pgStatStatementSharedBlksHit :: c12
    , pgStatStatementSharedBlksRead :: c13
    , pgStatStatementSharedBlksDirtied :: c14
    , pgStatStatementSharedBlksWritten :: c15
    , pgStatStatementLocalBlksHit :: c16
    , pgStatStatementLocalBlksRead :: c17
    , pgStatStatementLocalBlksDirtied :: c18
    , pgStatStatementLocalBlksWritten :: c19
    , pgStatStatementTempBlksRead :: c20
    , pgStatStatementTempBlksWritten :: c21
    , pgStatStatementBlkReadTime :: c22
    , pgStatStatementBlkWriteTime :: c23
    }

type PgStatStatement = PgStatStatement' (Maybe Int64) (Maybe Int64) (Maybe Int64) (Maybe Text) (Maybe Int64) (Maybe Double) (Maybe Double) (Maybe Double) (Maybe Double) (Maybe Double) (Maybe Int64) (Maybe Int64) (Maybe Int64) (Maybe Int64) (Maybe Int64) (Maybe Int64) (Maybe Int64) (Maybe Int64) (Maybe Int64) (Maybe Int64) (Maybe Int64) (Maybe Double) (Maybe Double)

type PgStatStatementReadColumns = PgStatStatement' (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGText)) (Column (Nullable PGInt8)) (Column (Nullable PGFloat8)) (Column (Nullable PGFloat8)) (Column (Nullable PGFloat8)) (Column (Nullable PGFloat8)) (Column (Nullable PGFloat8)) (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGInt8)) (Column (Nullable PGFloat8)) (Column (Nullable PGFloat8))

type PgStatStatementWriteColumns = PgStatStatement' (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGFloat8))) (Maybe (Column (Nullable PGFloat8))) (Maybe (Column (Nullable PGFloat8))) (Maybe (Column (Nullable PGFloat8))) (Maybe (Column (Nullable PGFloat8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGFloat8))) (Maybe (Column (Nullable PGFloat8)))

$(makeAdaptorAndInstance "pPgStatStatement" ''PgStatStatement')

pgStatStatementTable :: Table PgStatStatementWriteColumns PgStatStatementReadColumns
pgStatStatementTable = Table "pg_stat_statements" (pPgStatStatement
  PgStatStatement
    { pgStatStatementUserid = optional "userid"
    , pgStatStatementDbid = optional "dbid"
    , pgStatStatementQueryid = optional "queryid"
    , pgStatStatementQuery = optional "query"
    , pgStatStatementCalls = optional "calls"
    , pgStatStatementTotalTime = optional "total_time"
    , pgStatStatementMinTime = optional "min_time"
    , pgStatStatementMaxTime = optional "max_time"
    , pgStatStatementMeanTime = optional "mean_time"
    , pgStatStatementStddevTime = optional "stddev_time"
    , pgStatStatementRows = optional "rows"
    , pgStatStatementSharedBlksHit = optional "shared_blks_hit"
    , pgStatStatementSharedBlksRead = optional "shared_blks_read"
    , pgStatStatementSharedBlksDirtied = optional "shared_blks_dirtied"
    , pgStatStatementSharedBlksWritten = optional "shared_blks_written"
    , pgStatStatementLocalBlksHit = optional "local_blks_hit"
    , pgStatStatementLocalBlksRead = optional "local_blks_read"
    , pgStatStatementLocalBlksDirtied = optional "local_blks_dirtied"
    , pgStatStatementLocalBlksWritten = optional "local_blks_written"
    , pgStatStatementTempBlksRead = optional "temp_blks_read"
    , pgStatStatementTempBlksWritten = optional "temp_blks_written"
    , pgStatStatementBlkReadTime = optional "blk_read_time"
    , pgStatStatementBlkWriteTime = optional "blk_write_time"
    }
  )

---- TYPES FOR TABLE pghero_query_stats ----

data PgheroQueryStat' c1 c2 c3 c4 c5 c6 =
  PgheroQueryStat
    { pgheroQueryStatId :: c1
    , pgheroQueryStatDatabase :: c2
    , pgheroQueryStatQuery :: c3
    , pgheroQueryStatTotalTime :: c4
    , pgheroQueryStatCalls :: c5
    , pgheroQueryStatCapturedAt :: c6
    }

type PgheroQueryStat = PgheroQueryStat' Int32 (Maybe Text) (Maybe Text) (Maybe Double) (Maybe Int64) (Maybe LocalTime)

type PgheroQueryStatReadColumns = PgheroQueryStat' (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGFloat8)) (Column (Nullable PGInt8)) (Column (Nullable PGTimestamp))

type PgheroQueryStatWriteColumns = PgheroQueryStat' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGFloat8))) (Maybe (Column (Nullable PGInt8))) (Maybe (Column (Nullable PGTimestamp)))

$(makeAdaptorAndInstance "pPgheroQueryStat" ''PgheroQueryStat')

pgheroQueryStatTable :: Table PgheroQueryStatWriteColumns PgheroQueryStatReadColumns
pgheroQueryStatTable = Table "pghero_query_stats" (pPgheroQueryStat
  PgheroQueryStat
    { pgheroQueryStatId = optional "id"
    , pgheroQueryStatDatabase = optional "database"
    , pgheroQueryStatQuery = optional "query"
    , pgheroQueryStatTotalTime = optional "total_time"
    , pgheroQueryStatCalls = optional "calls"
    , pgheroQueryStatCapturedAt = optional "captured_at"
    }
  )

---- TYPES FOR TABLE plans ----

data Plan' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 =
  Plan
    { planId :: c1
    , planBillingSystemId :: c2
    , planName :: c3
    , planDescription :: c4
    , planHandle :: c5
    , planSetupFee :: c6
    , planAmount :: c7
    , planCreatedAt :: c8
    , planUpdatedAt :: c9
    , planBillingSystemGroupId :: c10
    , planService :: c11
    , planAccountId :: c12
    , planIntervalUnit :: c13
    , planIntervalCount :: c14
    , planTrialPeriodDays :: c15
    , planDeletedAt :: c16
    , planCurrency :: c17
    }

type Plan = Plan' Int32 (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Scientific) (Maybe Scientific) LocalTime LocalTime (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe LocalTime) (Maybe Text)

type PlanReadColumns = Plan' (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column PGTimestamp) (Column PGTimestamp) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGTimestamp)) (Column (Nullable PGText))

type PlanWriteColumns = Plan' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Column PGTimestamp) (Column PGTimestamp) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGText)))

$(makeAdaptorAndInstance "pPlan" ''Plan')

planTable :: Table PlanWriteColumns PlanReadColumns
planTable = Table "plans" (pPlan
  Plan
    { planId = optional "id"
    , planBillingSystemId = optional "billing_system_id"
    , planName = optional "name"
    , planDescription = optional "description"
    , planHandle = optional "handle"
    , planSetupFee = optional "setup_fee"
    , planAmount = optional "amount"
    , planCreatedAt = required "created_at"
    , planUpdatedAt = required "updated_at"
    , planBillingSystemGroupId = optional "billing_system_group_id"
    , planService = optional "service"
    , planAccountId = optional "account_id"
    , planIntervalUnit = optional "interval_unit"
    , planIntervalCount = optional "interval_count"
    , planTrialPeriodDays = optional "trial_period_days"
    , planDeletedAt = optional "deleted_at"
    , planCurrency = optional "currency"
    }
  )

---- TYPES FOR TABLE schema_migrations ----

data SchemaMigration' c1 =
  SchemaMigration
    { schemaMigrationVersion :: c1
    }

type SchemaMigration = SchemaMigration' Text

type SchemaMigrationReadColumns = SchemaMigration' (Column PGText)

type SchemaMigrationWriteColumns = SchemaMigration' (Column PGText)

$(makeAdaptorAndInstance "pSchemaMigration" ''SchemaMigration')

schemaMigrationTable :: Table SchemaMigrationWriteColumns SchemaMigrationReadColumns
schemaMigrationTable = Table "schema_migrations" (pSchemaMigration
  SchemaMigration
    { schemaMigrationVersion = required "version"
    }
  )

---- TYPES FOR TABLE sessions ----

data Session' c1 c2 c3 c4 c5 =
  Session
    { sessionId :: c1
    , sessionSessionId :: c2
    , sessionData :: c3
    , sessionCreatedAt :: c4
    , sessionUpdatedAt :: c5
    }

type Session = Session' Int32 Text (Maybe Text) (Maybe LocalTime) (Maybe LocalTime)

type SessionReadColumns = Session' (Column PGInt4) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp))

type SessionWriteColumns = Session' (Maybe (Column PGInt4)) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp)))

$(makeAdaptorAndInstance "pSession" ''Session')

sessionTable :: Table SessionWriteColumns SessionReadColumns
sessionTable = Table "sessions" (pSession
  Session
    { sessionId = optional "id"
    , sessionSessionId = required "session_id"
    , sessionData = optional "data"
    , sessionCreatedAt = optional "created_at"
    , sessionUpdatedAt = optional "updated_at"
    }
  )

---- TYPES FOR TABLE subscription_components ----

data SubscriptionComponent' c1 c2 c3 c4 c5 c6 =
  SubscriptionComponent
    { subscriptionComponentId :: c1
    , subscriptionComponentSubscriptionId :: c2
    , subscriptionComponentComponentId :: c3
    , subscriptionComponentQuantity :: c4
    , subscriptionComponentCreatedAt :: c5
    , subscriptionComponentUpdatedAt :: c6
    }

type SubscriptionComponent = SubscriptionComponent' Int32 (Maybe Int32) (Maybe Int32) (Maybe Int32) LocalTime LocalTime

type SubscriptionComponentReadColumns = SubscriptionComponent' (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column PGTimestamp) (Column PGTimestamp)

type SubscriptionComponentWriteColumns = SubscriptionComponent' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Column PGTimestamp) (Column PGTimestamp)

$(makeAdaptorAndInstance "pSubscriptionComponent" ''SubscriptionComponent')

subscriptionComponentTable :: Table SubscriptionComponentWriteColumns SubscriptionComponentReadColumns
subscriptionComponentTable = Table "subscription_components" (pSubscriptionComponent
  SubscriptionComponent
    { subscriptionComponentId = optional "id"
    , subscriptionComponentSubscriptionId = optional "subscription_id"
    , subscriptionComponentComponentId = optional "component_id"
    , subscriptionComponentQuantity = optional "quantity"
    , subscriptionComponentCreatedAt = required "created_at"
    , subscriptionComponentUpdatedAt = required "updated_at"
    }
  )

---- TYPES FOR TABLE subscriptions ----

data Subscription' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 =
  Subscription
    { subscriptionId :: c1
    , subscriptionBillingSystemId :: c2
    , subscriptionOrganizationId :: c3
    , subscriptionPlanId :: c4
    , subscriptionStatus :: c5
    , subscriptionCouponCode :: c6
    , subscriptionReferralCode :: c7
    , subscriptionBillingSystemCreatedAt :: c8
    , subscriptionBillingSystemUpdatedAt :: c9
    , subscriptionActivatedAt :: c10
    , subscriptionCanceledAt :: c11
    , subscriptionExpiresAt :: c12
    , subscriptionCurrentPeriodStartsAt :: c13
    , subscriptionCurrentPeriodEndsAt :: c14
    , subscriptionNextAssessmentAt :: c15
    , subscriptionTrialStartAt :: c16
    , subscriptionTrialEndAt :: c17
    , subscriptionCancelAtEndOfPeriod :: c18
    , subscriptionCancellationReason :: c19
    , subscriptionTotalMrr :: c20
    , subscriptionTotalRevenue :: c21
    , subscriptionOutstandingBalance :: c22
    , subscriptionSignupRevenue :: c23
    , subscriptionCreatedAt :: c24
    , subscriptionUpdatedAt :: c25
    , subscriptionPlanMrr :: c26
    , subscriptionComponentMrr :: c27
    , subscriptionMrrRecognizedAt :: c28
    , subscriptionCurrentMrr :: c29
    , subscriptionService :: c30
    , subscriptionAccountId :: c31
    , subscriptionCouponId :: c32
    , subscriptionReferral :: c33
    , subscriptionQuantity :: c34
    , subscriptionFlaggedAt :: c35
    , subscriptionFlaggedByUserId :: c36
    }

type Subscription = Subscription' Int32 (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe LocalTime) (Maybe LocalTime) (Maybe LocalTime) (Maybe LocalTime) (Maybe LocalTime) (Maybe LocalTime) (Maybe LocalTime) (Maybe LocalTime) (Maybe LocalTime) (Maybe LocalTime) (Maybe Bool) (Maybe Text) (Maybe Scientific) (Maybe Scientific) (Maybe Scientific) (Maybe Scientific) LocalTime LocalTime (Maybe Scientific) (Maybe Scientific) (Maybe LocalTime) (Maybe Scientific) (Maybe Int32) (Maybe Int32) (Maybe Int32) Bool (Maybe Int32) (Maybe LocalTime) (Maybe Int32)

type SubscriptionReadColumns = Subscription' (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGBool)) (Column (Nullable PGText)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column PGTimestamp) (Column PGTimestamp) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column (Nullable PGTimestamp)) (Column (Nullable PGNumeric)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column PGBool) (Column (Nullable PGInt4)) (Column (Nullable PGTimestamp)) (Column (Nullable PGInt4))

type SubscriptionWriteColumns = Subscription' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGBool))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Column PGTimestamp) (Column PGTimestamp) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Column PGBool) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGInt4)))

$(makeAdaptorAndInstance "pSubscription" ''Subscription')

subscriptionTable :: Table SubscriptionWriteColumns SubscriptionReadColumns
subscriptionTable = Table "subscriptions" (pSubscription
  Subscription
    { subscriptionId = optional "id"
    , subscriptionBillingSystemId = optional "billing_system_id"
    , subscriptionOrganizationId = optional "organization_id"
    , subscriptionPlanId = optional "plan_id"
    , subscriptionStatus = optional "status"
    , subscriptionCouponCode = optional "coupon_code"
    , subscriptionReferralCode = optional "referral_code"
    , subscriptionBillingSystemCreatedAt = optional "billing_system_created_at"
    , subscriptionBillingSystemUpdatedAt = optional "billing_system_updated_at"
    , subscriptionActivatedAt = optional "activated_at"
    , subscriptionCanceledAt = optional "canceled_at"
    , subscriptionExpiresAt = optional "expires_at"
    , subscriptionCurrentPeriodStartsAt = optional "current_period_starts_at"
    , subscriptionCurrentPeriodEndsAt = optional "current_period_ends_at"
    , subscriptionNextAssessmentAt = optional "next_assessment_at"
    , subscriptionTrialStartAt = optional "trial_start_at"
    , subscriptionTrialEndAt = optional "trial_end_at"
    , subscriptionCancelAtEndOfPeriod = optional "cancel_at_end_of_period"
    , subscriptionCancellationReason = optional "cancellation_reason"
    , subscriptionTotalMrr = optional "total_mrr"
    , subscriptionTotalRevenue = optional "total_revenue"
    , subscriptionOutstandingBalance = optional "outstanding_balance"
    , subscriptionSignupRevenue = optional "signup_revenue"
    , subscriptionCreatedAt = required "created_at"
    , subscriptionUpdatedAt = required "updated_at"
    , subscriptionPlanMrr = optional "plan_mrr"
    , subscriptionComponentMrr = optional "component_mrr"
    , subscriptionMrrRecognizedAt = optional "mrr_recognized_at"
    , subscriptionCurrentMrr = optional "current_mrr"
    , subscriptionService = optional "service"
    , subscriptionAccountId = optional "account_id"
    , subscriptionCouponId = optional "coupon_id"
    , subscriptionReferral = required "referral"
    , subscriptionQuantity = optional "quantity"
    , subscriptionFlaggedAt = optional "flagged_at"
    , subscriptionFlaggedByUserId = optional "flagged_by_user_id"
    }
  )

---- TYPES FOR TABLE transactions ----

data Transaction' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 =
  Transaction
    { transactionId :: c1
    , transactionBillingSystemId :: c2
    , transactionAccountId :: c3
    , transactionOrganizationId :: c4
    , transactionSubscriptionId :: c5
    , transactionPlanId :: c6
    , transactionComponentId :: c7
    , transactionTransactionType :: c8
    , transactionKind :: c9
    , transactionSuccess :: c10
    , transactionAmount :: c11
    , transactionStartingBalance :: c12
    , transactionEndingBalance :: c13
    , transactionMemo :: c14
    , transactionGateway :: c15
    , transactionGatewayTransactionId :: c16
    , transactionCardNumber :: c17
    , transactionCardExpiration :: c18
    , transactionCardType :: c19
    , transactionBillingSystemPaymentId :: c20
    , transactionBillingSystemCreatedAt :: c21
    , transactionCreatedAt :: c22
    , transactionUpdatedAt :: c23
    , transactionService :: c24
    , transactionInvoiceId :: c25
    }

type Transaction = Transaction' Int32 (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Bool) (Maybe Scientific) (Maybe Scientific) (Maybe Scientific) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe LocalTime) LocalTime LocalTime (Maybe Int32) (Maybe Int32)

type TransactionReadColumns = Transaction' (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGBool)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column (Nullable PGNumeric)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column PGTimestamp) (Column PGTimestamp) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type TransactionWriteColumns = Transaction' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGBool))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGNumeric))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamp))) (Column PGTimestamp) (Column PGTimestamp) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4)))

$(makeAdaptorAndInstance "pTransaction" ''Transaction')

transactionTable :: Table TransactionWriteColumns TransactionReadColumns
transactionTable = Table "transactions" (pTransaction
  Transaction
    { transactionId = optional "id"
    , transactionBillingSystemId = optional "billing_system_id"
    , transactionAccountId = optional "account_id"
    , transactionOrganizationId = optional "organization_id"
    , transactionSubscriptionId = optional "subscription_id"
    , transactionPlanId = optional "plan_id"
    , transactionComponentId = optional "component_id"
    , transactionTransactionType = optional "transaction_type"
    , transactionKind = optional "kind"
    , transactionSuccess = optional "success"
    , transactionAmount = optional "amount"
    , transactionStartingBalance = optional "starting_balance"
    , transactionEndingBalance = optional "ending_balance"
    , transactionMemo = optional "memo"
    , transactionGateway = optional "gateway"
    , transactionGatewayTransactionId = optional "gateway_transaction_id"
    , transactionCardNumber = optional "card_number"
    , transactionCardExpiration = optional "card_expiration"
    , transactionCardType = optional "card_type"
    , transactionBillingSystemPaymentId = optional "billing_system_payment_id"
    , transactionBillingSystemCreatedAt = optional "billing_system_created_at"
    , transactionCreatedAt = required "created_at"
    , transactionUpdatedAt = required "updated_at"
    , transactionService = optional "service"
    , transactionInvoiceId = optional "invoice_id"
    }
  )

---- TYPES FOR TABLE users ----

data User' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 =
  User
    { userId :: c1
    , userEmail :: c2
    , userEncryptedPassword :: c3
    , userResetPasswordToken :: c4
    , userResetPasswordSentAt :: c5
    , userRememberCreatedAt :: c6
    , userSignInCount :: c7
    , userCurrentSignInAt :: c8
    , userLastSignInAt :: c9
    , userCurrentSignInIp :: c10
    , userLastSignInIp :: c11
    , userConfirmationToken :: c12
    , userConfirmedAt :: c13
    , userConfirmationSentAt :: c14
    , userUnconfirmedEmail :: c15
    , userFailedAttempts :: c16
    , userUnlockToken :: c17
    , userLockedAt :: c18
    , userFirstName :: c19
    , userLastName :: c20
    , userPhone :: c21
    , userAccountId :: c22
    , userCreatedAt :: c23
    , userUpdatedAt :: c24
    , userInvitationToken :: c25
    , userInvitationCreatedAt :: c26
    , userInvitationSentAt :: c27
    , userInvitationAcceptedAt :: c28
    , userInvitationLimit :: c29
    , userInvitedById :: c30
    , userInvitedByType :: c31
    , userInvitationsCount :: c32
    , userReceiveSignupEmails :: c33
    , userReceiveChurnEmails :: c34
    , userReceiveWeeklyRecapEmails :: c35
    , userReceiveMonthlyRecapEmails :: c36
    , userSystemOwner :: c37
    }

type User = User' Int32 Text Text (Maybe Text) (Maybe LocalTime) (Maybe LocalTime) Int32 (Maybe LocalTime) (Maybe LocalTime) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe LocalTime) (Maybe LocalTime) (Maybe Text) Int32 (Maybe Text) (Maybe LocalTime) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) LocalTime LocalTime (Maybe Text) (Maybe LocalTime) (Maybe LocalTime) (Maybe LocalTime) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Int32) Bool Bool Bool Bool (Maybe Bool)

type UserReadColumns = User' (Column PGInt4) (Column PGText) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column PGInt4) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGText)) (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column PGTimestamp) (Column PGTimestamp) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column PGBool) (Column PGBool) (Column PGBool) (Column PGBool) (Column (Nullable PGBool))

type UserWriteColumns = User' (Maybe (Column PGInt4)) (Column PGText) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Column PGInt4) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGText))) (Column PGInt4) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Column PGTimestamp) (Column PGTimestamp) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Column PGBool) (Column PGBool) (Column PGBool) (Column PGBool) (Maybe (Column (Nullable PGBool)))

$(makeAdaptorAndInstance "pUser" ''User')

userTable :: Table UserWriteColumns UserReadColumns
userTable = Table "users" (pUser
  User
    { userId = optional "id"
    , userEmail = required "email"
    , userEncryptedPassword = required "encrypted_password"
    , userResetPasswordToken = optional "reset_password_token"
    , userResetPasswordSentAt = optional "reset_password_sent_at"
    , userRememberCreatedAt = optional "remember_created_at"
    , userSignInCount = required "sign_in_count"
    , userCurrentSignInAt = optional "current_sign_in_at"
    , userLastSignInAt = optional "last_sign_in_at"
    , userCurrentSignInIp = optional "current_sign_in_ip"
    , userLastSignInIp = optional "last_sign_in_ip"
    , userConfirmationToken = optional "confirmation_token"
    , userConfirmedAt = optional "confirmed_at"
    , userConfirmationSentAt = optional "confirmation_sent_at"
    , userUnconfirmedEmail = optional "unconfirmed_email"
    , userFailedAttempts = required "failed_attempts"
    , userUnlockToken = optional "unlock_token"
    , userLockedAt = optional "locked_at"
    , userFirstName = optional "first_name"
    , userLastName = optional "last_name"
    , userPhone = optional "phone"
    , userAccountId = optional "account_id"
    , userCreatedAt = required "created_at"
    , userUpdatedAt = required "updated_at"
    , userInvitationToken = optional "invitation_token"
    , userInvitationCreatedAt = optional "invitation_created_at"
    , userInvitationSentAt = optional "invitation_sent_at"
    , userInvitationAcceptedAt = optional "invitation_accepted_at"
    , userInvitationLimit = optional "invitation_limit"
    , userInvitedById = optional "invited_by_id"
    , userInvitedByType = optional "invited_by_type"
    , userInvitationsCount = optional "invitations_count"
    , userReceiveSignupEmails = required "receive_signup_emails"
    , userReceiveChurnEmails = required "receive_churn_emails"
    , userReceiveWeeklyRecapEmails = required "receive_weekly_recap_emails"
    , userReceiveMonthlyRecapEmails = required "receive_monthly_recap_emails"
    , userSystemOwner = optional "system_owner"
    }
  )

---- TYPES FOR TABLE webhook_subscriptions ----

data WebhookSubscription' c1 c2 c3 c4 c5 c6 c7 =
  WebhookSubscription
    { webhookSubscriptionId :: c1
    , webhookSubscriptionProvider :: c2
    , webhookSubscriptionUrl :: c3
    , webhookSubscriptionEvent :: c4
    , webhookSubscriptionAccountId :: c5
    , webhookSubscriptionCreatedAt :: c6
    , webhookSubscriptionUpdatedAt :: c7
    }

type WebhookSubscription = WebhookSubscription' Int32 (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) LocalTime LocalTime

type WebhookSubscriptionReadColumns = WebhookSubscription' (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column PGTimestamp) (Column PGTimestamp)

type WebhookSubscriptionWriteColumns = WebhookSubscription' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Column PGTimestamp) (Column PGTimestamp)

$(makeAdaptorAndInstance "pWebhookSubscription" ''WebhookSubscription')

webhookSubscriptionTable :: Table WebhookSubscriptionWriteColumns WebhookSubscriptionReadColumns
webhookSubscriptionTable = Table "webhook_subscriptions" (pWebhookSubscription
  WebhookSubscription
    { webhookSubscriptionId = optional "id"
    , webhookSubscriptionProvider = optional "provider"
    , webhookSubscriptionUrl = optional "url"
    , webhookSubscriptionEvent = optional "event"
    , webhookSubscriptionAccountId = optional "account_id"
    , webhookSubscriptionCreatedAt = required "created_at"
    , webhookSubscriptionUpdatedAt = required "updated_at"
    }
  )

---- TYPES FOR TABLE webhooks ----

data Webhook' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 =
  Webhook
    { webhookId :: c1
    , webhookService :: c2
    , webhookIdentifier :: c3
    , webhookEvent :: c4
    , webhookData :: c5
    , webhookAccountSlug :: c6
    , webhookCreatedAt :: c7
    , webhookUpdatedAt :: c8
    , webhookProcessedAt :: c9
    , webhookErrantAt :: c10
    , webhookRawData :: c11
    }

type Webhook = Webhook' Int32 (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe JSON.Value) (Maybe Text) LocalTime LocalTime (Maybe LocalTime) (Maybe LocalTime) (Maybe Text)

type WebhookReadColumns = Webhook' (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGJson)) (Column (Nullable PGText)) (Column PGTimestamp) (Column PGTimestamp) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGText))

type WebhookWriteColumns = Webhook' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGJson))) (Maybe (Column (Nullable PGText))) (Column PGTimestamp) (Column PGTimestamp) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGText)))

$(makeAdaptorAndInstance "pWebhook" ''Webhook')

webhookTable :: Table WebhookWriteColumns WebhookReadColumns
webhookTable = Table "webhooks" (pWebhook
  Webhook
    { webhookId = optional "id"
    , webhookService = optional "service"
    , webhookIdentifier = optional "identifier"
    , webhookEvent = optional "event"
    , webhookData = optional "data"
    , webhookAccountSlug = optional "account_slug"
    , webhookCreatedAt = required "created_at"
    , webhookUpdatedAt = required "updated_at"
    , webhookProcessedAt = optional "processed_at"
    , webhookErrantAt = optional "errant_at"
    , webhookRawData = optional "raw_data"
    }
  )

