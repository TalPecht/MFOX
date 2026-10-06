# MFOX email notifications

Decision emails use Resend and are deliberately separated from the curator decision transaction so an email/network failure cannot freeze the Fox Den.

## Configure
Set these environment variables before starting the app:

```
MFOX_RESEND_API_KEY=your_resend_api_key
MFOX_FROM_EMAIL=MFOX <curation@your-verified-domain.org>
```

The `MFOX_FROM_EMAIL` address/domain must be allowed by your Resend account. Restart R/Shiny after changing environment variables.

## Delivery
When a submitter opted in and a curator records a decision, the contribution is marked `Pending notification`. You can deliver queued messages from **Fox Den → Send pending emails**.

For automatic delivery in production, schedule:

```
Rscript scripts/send_pending_notifications.R
```

For example, run it every 5 minutes using cron, systemd, Posit Connect scheduling, or the scheduler provided by your deployment platform. The worker updates `notification_status` and `notified_at` in `data/contributions.csv`.

If the Den says **Email not configured**, outbound email cannot work until the two environment variables are set. This is an external service requirement, not a UI setting.
